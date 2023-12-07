(ns slackcommands.slack
  (:require [clj-slack.api :as api]
            [clj-slack.chat :as chat]
            [clj-slack.conversations :as conversations]
            [clj-slack.files :as files]
            [clj-slack.core :refer [slack-request]]
            [clj-http.client :as client]
            [slackcommands.ai.assistant :as assistant]
            [clojure.core.async :as async]
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

(defonce sent-msg-ids (atom #{}))
(defn events-api [req]
  (clojure.pprint/pprint req)
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
                 (->>
                  (tree-seq 
                   (fn [o]
                     (or (vector? o)
                         (map? o)))
                   (fn [o]
                     (cond
                       (vector? o) (seq o)
                       (map? o) (seq (get o "elements"))))))
                 (->> (filter #(#{"text" "link"} (get % "type")))
                      (map (fn [m]
                             (or (get m "url")
                                 (get m "text")))))
                 clojure.string/join)

        thread-id (if thread-ts
                    thread-ts
                    (get event "ts"))
        ch (async/chan)]
    (async/thread
      (prn "responding to" ch thread-id text)
      (assistant/respond ch thread-id text))
    (async/thread
      (let [placeholder-message
            (chat/post-message conn
                               channel
                               ":thonking:"
                               {"thread_ts" thread-id})]
        (loop [first? true]
          (when-let [{:keys [id text] :as msg} (async/<!! ch)]
            (when (:ts placeholder-message)
              (chat/delete conn
                           (:ts placeholder-message)
                           channel))
            (prn "got msg" msg)
            (let [[old _] (swap-vals! sent-msg-ids conj id)]
              (when (not (contains? old id))
                (doseq [chunk (partition-all 2999 text)
                        :let [subresponse (apply str chunk)]]
                  (chat/post-message conn channel
                                     subresponse
                                     {"thread_ts" thread-id
                                      "blocks" (json/write-str
                                                [{"type" "section"
                                                  "text" {"type" "mrkdwn"
                                                          "text" subresponse}}])})))
              (recur false))))))

    {;; :body (get js "challenge")
     :headers {"Content-type" "text/plain"}
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


