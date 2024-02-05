(ns slackcommands.ai
  (:require [clojure.java.io :as io]
            [wkok.openai-clojure.api :as api]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.core.memoize :as memo]
            [clj-http.client :as client]
            [slingshot.slingshot :refer [try+]]
            [slackcommands.util :as util]
            [slackcommands.stability :as stability]
            [slackcommands.slack :as slack]
            [slackcommands.clip :as clip]
            [clj-slack.chat :as slack-chat]
            [clojure.zip :as z]
            [clojure.edn :as edn]
            [com.phronemophobic.nubes :as nubes]
            [com.phronemophobic.discord.api :as discord])
  (:import java.util.concurrent.Executors))

(def ^:dynamic action-data nil)
(defn -get-action [s]
  (assert action-data)
  action-data)
(def one-day (* 1000 86400))
(defonce get-action (memo/ttl -get-action :ttl/threshold (* 2 one-day)))
(defn make-action [data]
  (binding [action-data data]
    (let [s (str "ai" (hash data))]
      (get-action s)
      s)))

(def api-key (:chatgpt/api-key
              (edn/read-string (slurp "secrets.edn"))))

(defn send-error-msg [response-url err]
  (let [{:keys [body]} err]
    (let [msg (try
                (let [payload (json/read-str body)]
                  (or 
                   (get-in payload ["message"])
                   (get-in payload ["error" "message"])))
                (catch Exception e
                  "Unknown Error"))]
      (clojure.pprint/pprint body)
      (let [blocks [{"type" "section"
                     "text" {"type" "plain_text"
                             "emoji" true
                             "text" (str ":whale: " msg)}}]]
        (client/post response-url
                     {:body (json/write-str
                             {
                              "response_type" "in_channel",
                              "blocks" blocks
                              "replace_original" true})
                      :headers {"Content-type" "application/json"}})))))

(defmacro wrap-exception [response-url & body]
  `(let [response-url# ~response-url]
     (try+
       ~@body
       (catch [:status 500] err#
         (send-error-msg err#))
       (catch [:status 429] err#
         (send-error-msg err#))
       (catch [:status 400] {body# :body}

         (let [msg# (try
                      (let [payload# (json/read-str body#)]
                        (get-in payload# ["error" "message"]))
                      (catch Exception e#
                        "Unknown Error"))]
           (client/post response-url#
                        {:body (json/write-str
                                {
                                 "response_type" "in_channel",
                                 "blocks"
                                 [{"type" "section"
                                   "text" {"type" "plain_text"
                                           "emoji" true
                                           "text" (str ":shame: :frogsiren: " msg#)}}]
                                 "replace_original" true})
                         :headers {"Content-type" "application/json"}})))
       (catch [:error :timeout] m#
         (client/post response-url#
                      {:body (json/write-str
                              {
                               "response_type" "in_channel",
                               "blocks"
                               [{"type" "section"
                                 "text" {"type" "plain_text"
                                         "emoji" true
                                         "text" (str ":frogsiren: timeout for \"" (:prompt m#) "\"")}}]
                               "replace_original" true})
                       :headers {"Content-type" "application/json"}}))
       (catch :message m#
         (client/post response-url#
                      {:body (json/write-str
                              {
                               "response_type" "in_channel",
                               "blocks"
                               [{"type" "section"
                                 "text" {"type" "plain_text"
                                         "emoji" true
                                         "text" (str ":frogsiren: :" (:message m#))}}]
                               "replace_original" true})
                       :headers {"Content-type" "application/json"}}))
       (catch Exception e#
         (client/post response-url#
                      {:body (json/write-str
                              {
                               "response_type" "in_channel",
                               "blocks"
                               [{"type" "section"
                                 "text" {"type" "plain_text"
                                         "emoji" true
                                         "text" (str ":frogsiren: :" (ex-message e#))}}]
                               "replace_original" true})
                       :headers {"Content-type" "application/json"}}))
       (catch Object e#
         (clojure.pprint/pprint e#)
         (client/post response-url#
                      {:body (json/write-str
                              {
                               "response_type" "in_channel",
                               "blocks"
                               [{"type" "section"
                                 "text" {"type" "plain_text"
                                         "emoji" true
                                         "text" (str ":frogsiren: :" "Unknown Error")}}]
                               "replace_original" true})
                       :headers {"Content-type" "application/json"}})))))


(defmacro wrap-exception2 [response-info placeholder & body]
  `(let [
         response-info# ~response-info
         channel-id# (get response-info# :channel-id)
         response-url# (get response-info# :response-url)
         response-ts# (get response-info# :ts)

         placeholder-response# 
         (slack-chat/post-message slack/conn
                                  channel-id#
                                  ~placeholder
                                  (when response-ts#
                                    {"thread_ts" response-ts#}))
         ts# (get placeholder-response# :ts)
         ~'send-update (when ts#
                         (let [first-atm# (atom true)]
                           (fn [msg#]
                             (let [[first?# _#] (swap-vals! first-atm# (constantly false))]
                               (if first?#
                                 (slack/message-update slack/conn
                                                       channel-id#
                                                       ts#
                                                       msg#)
                                 (slack-chat/post-message slack/conn
                                                          channel-id#
                                                          response-ts#
                                                          msg#))))))
         ~'send-new (when (not ts#)
                      (fn [msg#]
                        (client/post
                         response-url#
                         {:body (json/write-str
                                 (merge msg#
                                        (when response-ts#
                                          {"thread_ts" response-ts#})))
                          :headers {"Content-type" "application/json"}})))
         ~'send-blocks
         (if ts#
           (fn [blocks#]
             (~'send-update {"blocks" 
                             (json/write-str blocks#)}))
           (fn [blocks#]
             (client/post
              response-url#
              {:body (json/write-str
                      (merge
                       {"response_type" "in_channel",
                        "blocks" blocks#
                        "replace_original" false
                        }
                       (when response-ts#
                         {"thread_ts" response-ts#})))
               :headers {"Content-type" "application/json"}})))]
     (try+
       ~@body
       (catch [:error :timeout] m#
         (clojure.pprint/pprint m#)
         (~'send-blocks
          [{"type" "section"
            "text" {"type" "plain_text"
                    "emoji" true
                    "text" (str ":frogsiren: timeout for \"" (:prompt m#) "\"")}}]))
       (catch :message m#
         (clojure.pprint/pprint m#)
         (~'send-blocks [{"type" "section"
                          "text" {"type" "plain_text"
                                  "emoji" true
                                  "text" (str ":frogsiren: :" (:message m#))}}]))
       (catch Exception e#
         (clojure.pprint/pprint e#)
         (~'send-blocks [{"type" "section"
                          "text" {"type" "plain_text"
                                  "emoji" true
                                  "text" (str ":frogsiren: :" (ex-message e#))}}]))
       (catch Object e#
         (clojure.pprint/pprint e#)
         (~'send-blocks [{"type" "section"
                          "text" {"type" "plain_text"
                                  "emoji" true
                                  "text" (str ":frogsiren: :" "Unknown Error")}}])))))

(defn chat-completion [ai-type messages]
  (case ai-type
    :naighty
    (nubes/run-naighty messages)

    ;; default
    (let [response (api/create-chat-completion {:model
                                                "gpt-4"
                                                ;; "gpt-3.5-turbo"
                                                :messages messages}
                                               {:api-key api-key})
          message (-> response
                      :choices
                      first
                      :message)]
      message)))

(defn send-chat-response
  [{:keys [response-url thread-ts messages text channel-id ai-type]}]
  (let [messages (or messages [])]
    (when (seq (clojure.string/trim text))
      (future
        (wrap-exception2
            {:response-url response-url
             :ts thread-ts
             :channel-id channel-id}
            (str (rand-nth [":thonktopus:"
                            ":thonk:"
                            ":thonking_intensifies:"
                            ":thonking:"
                            ":thonk-spin:"
                            ":thonk-zoom:"
                            ":thonk-pirate:"
                            ":thonkest-spin:"])
                 " "
                 text)
          (let [messages (conj messages {:role "user" :content text})
                message (chat-completion ai-type messages)

                full-response (clojure.string/join "\n\n"
                                                   (into []
                                                         (comp (map (fn [{:keys [role content]}]
                                                                      (case role
                                                                        "user" (str "*" content "*")
                                                                        "assistant"  content))))
                                                         (take-last 2
                                                                    (conj messages
                                                                          message))))]
            
            (doseq [chunk (partition-all 2999 full-response)
                    :let [subresponse (apply str chunk)]]
              (send-blocks [{"type" "section"
                             "text" {"type" "mrkdwn"
                                     "text" subresponse}}
                            {
                             "dispatch_action" true,
                             "type" "input",
                             "element" {
                                        "type" "plain_text_input",
                                        "action_id"
                                        (util/make-action
                                         `chat-more-interact
                                         {:messages (conj messages message)
                                          :ai-type ai-type})},
                             "label" {
                                      "type" "plain_text",
                                      "text" "yes, and?",
                                      "emoji" true
                                      }
                             }
                            ])))))))
  )

(defn chat-command [request]
  ;; gpt-3.5-turbo
  (let [text (get-in request [:form-params "text"])
        channel-id (get-in request [:form-params "channel_id"])
        response-url (get-in request [:form-params "response_url"])]
    (println "chat" ": " text)
    (send-chat-response
     {:response-url response-url
      :channel-id channel-id
      :ai-type (:ai-type request)
      :text text})

    {:body (json/write-str
            {"response_type" "in_channel"
             }) 
     :headers {"Content-type" "application/json"}
     :status 200}))



(defn image-response [prompt urls index]
  (let [url (nth urls index)
        main-blocks [ {"type" "image",
                       "title" {"type" "plain_text",
                                "text" (str prompt " (" (inc index) "/" (count urls) ")")},
                       "image_url" url
                       "alt_text" prompt}]]
    {"response_type" "in_channel"
     "blocks"
     (into main-blocks
           [{"type" "divider"}
            {"type" "actions",
             "elements"
             [{"type" "button",
               "text"
               {"type" "plain_text", "text" "<<"}
               "value"
               (util/make-action `get-image-interact
                                 {:prompt prompt
                                  :urls urls
                                  :index (mod (dec index) (count urls))})
               #_(make-action [:get-image prompt urls (mod (dec index) (count urls))])}
              {"type" "button",
               "text"
               {"type" "plain_text", "text" ">>"}
               "value" #_(make-action [:get-image prompt urls (mod (inc index) (count urls))])
               (util/make-action `get-image-interact
                                 {:prompt prompt
                                  :urls urls
                                  :index (mod (inc index) (count urls))})}]}])})
  )

(defn truncate-from-end [s n]
  (subs s (max 0 (- (count s)
                    n))
        (count s)))
(defn midjourney-image-response [prompt url section]
  (let [title (truncate-from-end prompt 82)
        main-blocks [{"type" "image",
                      "title" {"type" "plain_text",
                               "text" (str title " ("section ")")},
                      "image_url" url
                      "alt_text" (str title " ("section ")")}]]
    {"response_type" "in_channel"
     "blocks" main-blocks}))

(defn dalle3-image-response [prompt url]
  (let [title (truncate-from-end prompt 82)
        main-blocks [{"type" "image",
                      "title" {"type" "plain_text",
                               "text" title},
                      "image_url" url
                      "alt_text" title}]]
    {"response_type" "in_channel"
     "blocks" main-blocks}))

(defn block-zip [blocks]
  (z/zipper vector?
            seq
            (fn [v children]
              (into (empty v) children))
            blocks))

(defn reset-chat-text-input [blocks]
  (let [zip (block-zip blocks)]
    (loop [zip zip]
      (if (z/end? zip)
        (z/root zip)
        ;; else
        (let [m (z/node zip)
              new-zip (if (= "input" (get m "type"))
                        (z/replace zip (dissoc m "block_id"))
                        zip)]
          (recur (z/next new-zip))))))
  )

(defn chat-more-interact [payload {:keys [messages ai-type]}]
  (let [url (get payload "response_url")
        channel-id (get-in payload ["channel" "id"])
        ts (get-in payload ["message" "thread_ts"]
                   (get-in payload ["message" "ts"]))
        action (-> payload
                   (get "actions")
                   first
                   (get "value"))]
    (when (not= action "image")
      (future
        (send-chat-response
         {:response-url url
          :thread-ts ts
          :channel-id channel-id
          :ai-type ai-type
          :messages messages
          :text action})
        (let [blocks (get-in payload ["message" "blocks"])]
          (client/post url
                       {:body (json/write-str
                               {"blocks" (reset-chat-text-input blocks)
                                "response_type" "in_channel"
                                "replace_original" true})
                        :headers {"Content-type" "application/json"}}))))

    {:status 200}))

(defn get-image-interact [payload {:keys [prompt urls index] }]
  (let [url (get payload "response_url")]
    (future
      (try
        (client/post url
                     {:body (json/write-str
                             (assoc (image-response prompt urls index)
                                    "replace_original" true))
                      :headers {"Content-type" "application/json"}})
        (catch Exception e
          (prn e)))))
  {:body "ok"
   :headers {"Content-type" "application/json"}
   :status 200})

(defn image-command [request]
  (let [text (get-in request [:form-params "text"])
        response-url (get-in request [:form-params "response_url"])]
    (println "image: " text)
    (when (seq (clojure.string/trim text))
      (future
        (wrap-exception
            response-url
          (let [response (api/create-image {:prompt text
                                            :n 1
                                            :model "dall-e-3"
                                            :size
                                            ;; "256x256"
                                            ;; "512x512"
                                            "1024x1024"
                                            }
                                           {:api-key api-key})

                response-id (str (random-uuid))

                urls (into []
                           (comp (map :url)
                                 (map-indexed
                                  (fn [i url]
                                    (util/save-and-upload-large-png url))))
                           (:data response))]

            (client/post response-url
                         {:body (json/write-str
                                 (dalle3-image-response text (first urls)))
                          :headers {"Content-type" "application/json"}})))))

    {:body (json/write-str
            {"response_type" "in_channel"
             })
     :headers {"Content-type" "application/json"}
     :status 200}))

(defn stable-image-command [request]
  
  (let [text (get-in request [:form-params "text"])
        response-url (get-in request [:form-params "response_url"])]

    (if (#{"" "help"} (str/trim text))
      {:body (json/write-str
              {"response_type" "in_channel"
               "blocks" [{"type" "section"
                          "text" {"type" "mrkdwn"
                                  "text" (str "```\n" (stability/help-text) "\n```")}}]})
       :headers {"Content-type" "application/json"}
       :status 200}
      ;; else
      (do
        (println "stable image: " text)
        (when (seq (clojure.string/trim text))
          (future
            (wrap-exception
                response-url
              (let [urls (stability/create-image text)]
                (client/post response-url
                             {:body (json/write-str
                                     (image-response text urls 0))
                              :headers {"Content-type" "application/json"}})))))
        {:body (json/write-str
                {"response_type" "in_channel"})
         :headers {"Content-type" "application/json"}
         :status 200}))))


(defn midjourney-help-text []
  (str
   "Usage: /midjourney [prompt]

See <https://docs.midjourney.com/docs/models> for more options.
"))

(defn augment-midjourney-prompt [text]
  (-> text
      (str/replace #"—" "--")
      (str/replace #"[\n\r]+" " ")
      (str/replace #"--v 6" "--v 6.0")
      (str/replace #"[”“]" "\"")
      (str/replace #"\s*::\s*" ":: ")
      (str/trim)))

(defonce resize-image-executor
  (delay
    (let [thread-factory
          (reify
            java.util.concurrent.ThreadFactory
            (newThread [this r]
              (let [thread (.newThread (Executors/defaultThreadFactory)
                                       r)]
                ;; set priority to one less than normal
                (.setPriority thread
                              (max Thread/MIN_PRIORITY
                                   (dec Thread/NORM_PRIORITY)))
                thread)))]
      (Executors/newSingleThreadExecutor thread-factory))))
(defn split-large-png [url]
  @(.submit
    ^java.util.concurrent.ExecutorService
    @resize-image-executor
    ^java.util.concurrent.Callable
    (fn []
      (util/split-large-png url))))

(defn midjourney-image-command [request]
  (let [text (get-in request [:form-params "text"])
        channel-id (get-in request [:form-params "channel_id"])
        response-url (get-in request [:form-params "response_url"])]

    (if (#{"" "help"} (str/trim text))
      {:body (json/write-str
              {"response_type" "in_channel"
               "blocks" [{"type" "section"
                          "text" {"type" "mrkdwn"
                                  "text" (str "```\n" (midjourney-help-text) "\n```")}}]})
       :headers {"Content-type" "application/json"}
       :status 200}
      ;; else
      (do
        (println "midjourney: " text)
        (when (seq (clojure.string/trim text))
          (future
            (wrap-exception2
                {:channel-id channel-id
                 :response-url response-url}
                (str ":waitingcat: " text)
              (let [prompt (augment-midjourney-prompt text)
                    response (discord/create-image prompt)]
                (if-let [url (:url response)]
                  (let [title (truncate-from-end text 82)]
                    (if send-update
                      (let [img-urls (util/split-large-png url)]
                        (send-update
                         {"attachments"
                          (json/write-str 
                           (into []
                                 (map
                                  (fn [[i url]]
                                    {
                                     "fallback" (str title "("i ")")
                                     "image_url" url
                                     "footer" (str title "(" i ")")}))
                                 (map vector ["top" "bottom"] img-urls)))}))
                      (let [[top bottom] (split-large-png url)]
                        (send-new (midjourney-image-response text top "top"))
                        (send-new (midjourney-image-response text bottom "bottom")))))
                  (throw (ex-info "Error" response)))))))

        {:body (json/write-str
                {"response_type" "in_channel"})
         :headers {"Content-type" "application/json"}
         :status 200}))))

(defn find-nearest [text]
  @(.submit
    ^java.util.concurrent.ExecutorService
    @resize-image-executor
    ^java.util.concurrent.Callable
    (fn []
      (clip/find-nearest text))))

(defn seairch-command [request]
  (let [text (get-in request [:form-params "text"])
        response-url (get-in request [:form-params "response_url"])]

    (if (#{"" "help"} (str/trim text))
      {:body (json/write-str
              {"response_type" "in_channel"
               "blocks" [{"type" "section"
                          "text" {"type" "mrkdwn"
                                  "text" (str "```\n/seairch <query>\n```")}}]})
       :headers {"Content-type" "application/json"}
       :status 200}
      ;; else
      (do
        (println "image search: " text)
        (when (seq (clojure.string/trim text))
          (future
            (wrap-exception
                response-url
              (let [image-names (find-nearest text)
                    urls (into []
                               (map (fn [fname]
                                      (str "https://" "aimages.smith.rocks/" fname)))
                               image-names)]
                (client/post response-url
                             {:body (json/write-str
                                     (image-response text urls 0))
                              :headers {"Content-type" "application/json"}})))))
        {:body (json/write-str
                {"response_type" "in_channel"})
         :headers {"Content-type" "application/json"}
         :status 200}))))

(comment
  (def response
    
    (api/create-image {:prompt "An oracle visiting the real monsters tribe"
                       :n 5
                       :size "512x512"}
                      {:api-key api-key})

    (def response (discord/create-image text))
    
    ,)

  (def imgs
    (into []
          (comp (map :url)
                (map io/as-url)
                (map ui/image))
          (:data response))
    )

  (require '[membrane.skia :as skia]
           '[membrane.ui :as ui])

  (skia/run #'debug)
  ,)
