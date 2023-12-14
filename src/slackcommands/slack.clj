(ns slackcommands.slack
  (:require [clj-slack.core :refer [slack-request]]
            [clj-slack.conversations :as conversations]
            [clj-slack.chat :as chat]
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

(def main-channel-id
  (delay
    (->> (conversations/list conn 
                             {"types" "public_channel"})
         :channels
         (filter #(= "shaitposting"
                     (:name %)))
         first
         :id
         )))

(defn send-to-main [mdown]
  (chat/post-message
   conn
   @main-channel-id
   mdown
   {"blocks" (json/write-str
              [{"type" "section"
                "text" {"type" "mrkdwn"
                        "text" mdown}}])}))

(comment
  (conversations/list conn 
                     {"types" "im"} )

  (def channels
    (conversations/list conn 
                       {"types" "public_channel"} ))

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


