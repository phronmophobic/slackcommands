(ns slackcommands.ai
  (:require [clojure.java.io :as io]
            [wkok.openai-clojure.api :as api]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.core.memoize :as memo]
            [clj-http.client :as client]
            [clojure.edn :as edn]))

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

(defn send-chat-response
  [{:keys [response-url thread-ts messages text]}]
  (let [messages (or messages [])]
    (when (seq (clojure.string/trim text))
      (future
        (let [messages (conj messages {:role "user" :content text})
              response (api/create-chat-completion {:model "gpt-3.5-turbo"
                                                    :messages messages}
                                                   {:api-key api-key})
              message (-> response
                          :choices
                          first
                          :message)
              chat-response (:content message)

              full-response (clojure.string/join "\n\n"
                                                 (into []
                                                       (comp (map (fn [{:keys [role content]}]
                                                                    (case role
                                                                      "user" (str "*" content "*")
                                                                      "assistant"  content))))
                                                       (take-last 2
                                                                  (conj messages
                                                                        message))))]
          (try
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
                            :headers {"Content-type" "application/json"}}))
            (catch Exception e
              (prn "message length:" (count full-response))
              (prn e)))))))
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

           {:body "ok"
            :headers {"Content-type" "application/json"}
            :status 200})))

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
      nil)))

(def aimage-dir
  (doto (io/file "aimages")
    (.mkdirs)))

(defn save-image [fname url]
  (let [f (io/file aimage-dir fname)]
    (with-open [is (io/input-stream (io/as-url url))]
      (io/copy is
               f)))
  nil)

(defn image-command [request]
  (let [text (get-in request [:form-params "text"])
        response-url (get-in request [:form-params "response_url"])]
    (println "image: " text)
    (when (seq (clojure.string/trim text))
      (future
        (let [response (api/create-image {:prompt text
                                          :n 4
                                          :size
                                          "256x256"
                                          ;; "512x512"
                                          ;; "1024x1024"
                                          }
                                         {:api-key api-key})

              response-id (str (random-uuid))
              ;; image-url (-> response
              ;;               :data
              ;;               first
              ;;               :url)
              host (:server-name request)
              ;; slack won't show preview images from just any port
              port 3000
              urls (into []
                         (comp (map :url)
                               (map-indexed
                                (fn [i url]
                                  (let [fname (str response-id "-" i ".png")]
                                    (save-image fname
                                                url)
                                    fname)))
                               (map (fn [fname]
                                      (str "http://" host ":" port "/aimages/" fname))))
                         (:data response))]
          (try
            (client/post response-url
                         {:body (json/write-str
                                 (image-response text urls 0)
                                 #_{
                                  "response_type" "in_channel",
                                  "blocks"
                                  [{
                                    "type" "image",
                                    "image_url" image-url
                                    "alt_text" text
                                    ;; "text" image-url
                                    }
                                   ]

                                  "replace_original" true})
                          :headers {"Content-type" "application/json"}})
            (catch Exception e
              (prn e))))))

    {:body (json/write-str
            {"response_type" "in_channel"
             })
     :headers {"Content-type" "application/json"}
     :status 200}))

(comment
  (def response
    
  (api/create-image {:prompt "An oracle visiting the real monsters tribe"
                     :n 5
                     :size "512x512"}
                    {:api-key api-key})

    (def response
      )
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




