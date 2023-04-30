(ns slackcommands.ai
  (:require [clojure.java.io :as io]
            [wkok.openai-clojure.api :as api]
            [clojure.data.json :as json]
            [clj-http.client :as client]
            [clojure.edn :as edn]))



(def api-key (:chatgpt/api-key
              (edn/read-string (slurp "secrets.edn"))))


(defn chat-command [request]
  ;; gpt-3.5-turbo
  (let [text (get-in request [:form-params "text"])
        response-url (get-in request [:form-params "response_url"])]

    (when (seq (clojure.string/trim text))
      (future
        (let [response (api/create-chat-completion {:model "gpt-3.5-turbo"
                                                    :messages [{:role "user" :content text}]}
                                                   {:api-key api-key})
              chat-response (-> response
                                :choices
                                first
                                :message
                                :content)]
          (try
            (client/post response-url
                         {:body (json/write-str
                                 {
                                  "response_type" "in_channel",
                                  "text" chat-response

                                  "replace_original" "true"})
                          :headers {"Content-type" "application/json"}})
            (catch Exception e
              (prn e))))))

    {:body (json/write-str
            {"response_type" "in_channel"
             "blocks" [{"type" "section"
                        #_#_"text" {"type" "plain_text"
                                "emoji" true
                                "text" ":waitingcat:"}}]}) 
     :headers {"Content-type" "application/json"}
     :status 200}


    )
)

(defn image-command [request]
  (let [text (get-in request [:form-params "text"])
        response-url (get-in request [:form-params "response_url"])]

    (when (seq (clojure.string/trim text))
      (future
        (let [response (api/create-image {:prompt text
                                          :n 1
                                          :size "512x512"}
                                         {:api-key api-key})
              image-url (-> response
                            :data
                            first
                            :url)]

          (try
            (client/post response-url
                         {:body (json/write-str
                                 {
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
             #_#_"blocks" [{"type" "section"
                        "text" {"type" "plain_text"
                                "emoji" true
                                "text" ":waitingcat:"}}]})
     :headers {"Content-type" "application/json"}
     :status 200}


    ))

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




