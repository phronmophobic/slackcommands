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
            [clj-slack.chat :as slack-chat]
            [clojure.zip :as z]
            [clojure.edn :as edn]
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

(def ^:dynamic *retry* nil)

(defmacro wrap-retry [& body]
  `(let [f# (fn retry# []
              (binding [*retry* retry#]
                ~@body))]
     (f#)))

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
      (let [retry-button {"type" "actions",
                           "elements"
                           [{"type" "button",
                             "text"
                             {"type" "plain_text", "text" "try again"}
                             "value" (make-action [:retry *retry*])}]}
            blocks [{"type" "section"
                      "text" {"type" "plain_text"
                              "emoji" true
                              "text" (str ":whale: " msg)}}]
            blocks (if *retry*
                      (conj blocks retry-button)
                      blocks)]
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

(defn send-chat-response
  [{:keys [response-url thread-ts messages text]}]
  (let [messages (or messages [])]
    (when (seq (clojure.string/trim text))
      (future
        (wrap-exception
         response-url
         (let [messages (conj messages {:role "user" :content text})
               response (api/create-chat-completion {:model
                                                     "gpt-4"
                                                     ;; "gpt-3.5-turbo"
                                                     :messages messages}
                                                    {:api-key api-key})
               message (-> response
                           :choices
                           first
                           :message)

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
             (client/post response-url
                          {:body (json/write-str
                                  (merge
                                   {
                                    "response_type" "in_channel",
                                    "blocks"
                                    [{"type" "section"
                                      "text" {"type" "mrkdwn"
                                              "text" subresponse}}
                                     {
                                      "dispatch_action" true,
                                      "type" "input",
                                      "element" {
                                                 "type" "plain_text_input",
                                                 "action_id" (make-action
                                                              [:chat-more
                                                               (conj messages message)])
                                                 },
                                      "label" {
                                               "type" "plain_text",
                                               "text" "yes, and?",
                                               "emoji" true
                                               }
                                      }
                                     ]
                                    ;; "thread_ts" thread-ts
                                    "replace_original" false}
                                   (when thread-ts
                                     {"thread_ts" thread-ts}))
                                  )
                           :headers {"Content-type" "application/json"}})))))))
  )

(defn chat-command [request]
  ;; gpt-3.5-turbo
  (let [text (get-in request [:form-params "text"])
        response-url (get-in request [:form-params "response_url"])
        ]
    (println "chat" ": " text)
    (send-chat-response
     {:response-url response-url
      :text text})

    {:body (json/write-str
            {"response_type" "in_channel"
             }) 
     :headers {"Content-type" "application/json"}
     :status 200}


    )
)



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
               "value" (make-action [:get-image prompt urls (mod (dec index) (count urls))])}
              {"type" "button",
               "text"
               {"type" "plain_text", "text" ">>"}
               "value" (make-action [:get-image prompt urls (mod (inc index) (count urls))])}]}])})
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

(defn ai-command-interact [request]
  (let [payload (json/read-str (get (:form-params request) "payload"))
        url (get payload "response_url")
        action (-> payload
                   (get "actions")
                   first
                   (get "value"))
        action-id (-> payload
                      (get "actions")
                      first
                      (get "action_id"))
        [action-type & action-args :as event]
        (cond
          (.startsWith action "ai")
          (get-action action)

          (.startsWith action-id "ai")
          (get-action action-id))]
    (case action-type

      :retry
      (let [[_ retry-fn] event]
        
        (future
          (try
            (client/post url
                         {:body (json/write-str
                                 {"delete_original" true})
                          :headers {"Content-type" "application/json"}})
            (retry-fn)
            (catch Exception e
              (prn e))))
        {:body "ok"
         :headers {"Content-type" "application/json"}
         :status 200})

      :chat-more
      (let [[_ messages] event]
        (if (= action "image")
          (do
            "convert to image prompt"
            )
         (let [ts (get-in payload ["message" "thread_ts"]
                          (get-in payload ["message" "ts"]))]
           (send-chat-response
            {:response-url url
             :thread-ts ts
             :messages messages
             :text action})
           (future
             (let [blocks (get-in payload ["message" "blocks"])]
              (client/post url
                           {:body (json/write-str
                                   {"blocks" (reset-chat-text-input blocks)
                                    "response_type" "in_channel"
                                    "replace_original" true})
                            :headers {"Content-type" "application/json"}})))

           {:status 200})))

      :get-image
      (let [[_ prompt urls index] event]
        (future
          (try
            (client/post url
                         {:body (json/write-str
                                 (assoc (image-response prompt urls index)
                                        "replace_original" true))
                          :headers {"Content-type" "application/json"}})
            (catch Exception e
              (prn e))))
        {:body "ok"
         :headers {"Content-type" "application/json"}
         :status 200})

      ;; else
      )))



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
            (wrap-retry
             (wrap-exception
              response-url
              (let [urls (stability/create-image text)]
                (client/post response-url
                             {:body (json/write-str
                                     (image-response text urls 0))
                              :headers {"Content-type" "application/json"}}))))))
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
            (let [{:keys [ts]
                   :as msg} 
                  (slack-chat/post-message 
                   slack/conn
                   channel-id 
                   (str ":waitingcat: " text))]
              (wrap-exception
                  response-url
                (let [prompt (augment-midjourney-prompt text)
                      response (discord/create-image prompt)]
                  (if-let [url (:url response)]
                    (let [
                          title (truncate-from-end text 82)]
                      (if ts
                        (let [img-urls (util/split-large-png url)]
                          (slack/message-update 
                           slack/conn
                           channel-id
                           ts
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
                          (client/post response-url
                                       {:body (json/write-str
                                               (midjourney-image-response text top "top"))
                                        :headers {"Content-type" "application/json"}})
                          (client/post response-url
                                       {:body (json/write-str
                                               (midjourney-image-response text bottom "bottom"))
                                        :headers {"Content-type" "application/json"}}))))
                    (throw (ex-info "Error" response))))))))

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
