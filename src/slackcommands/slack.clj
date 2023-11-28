(ns slackcommands.slack
  (:require [clj-slack.api :as api]
            [clj-slack.chat :as chat]
            [clj-slack.conversations :as conversations]
            [clj-slack.files :as files]
            [clj-slack.core :refer [slack-request]]
            [clj-http.client :as client]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.edn :as edn]))

(def slack-oauth-token
  (:slack/oauth-token
   (edn/read-string (slurp "secrets.edn"))))

(def conn 
  {:api-url "https://slack.com/api" :token slack-oauth-token})

(defn message-update [connection channel-id timestamp opts]
  (slack-request connection "chat.update" 
                 (merge {"ts" timestamp "channel" channel-id}
                        opts)))


(defn events-api [req]
  ;; (clojure.pprint/pprint req)
  (let [js (with-open [body (:body req)
                       rdr (io/reader body)]
             (json/read rdr))
        event (get js "event")
        thread-ts (get event "thread_ts")
        
        channel (get event "channel")
        text (-> event
                 (get "blocks")
                 first
                 (get "elements")
                 (->> (filter #(= "text" (get % "type")))))]
    ;; (clojure.pprint/pprint js)
    (chat/post-message conn channel
                     "hullo"
                     (merge
                      {}
                      {"thread_ts" 
                       (if thread-ts
                         thread-ts
                         (get event "ts"))}))
    {;;:body (get js "challenge")
     ;; :headers {"Content-type" "text/plain"}
       :status 200}))

(comment
  (conversations/list conn 
                     {"types" "im"} )

  ;; File upload is broken
  (files/upload
   conn
   (io/file "aimages" "fffa9cbc-12cc-4b96-afb4-1b8c49445cc4.jpg")
   {"channels" "askdlfjasdfj" }
   )

  
  (slack-chat/post-message 
   slack/conn
   channel-id 
   ":waitingcat:")

  

  
,)


