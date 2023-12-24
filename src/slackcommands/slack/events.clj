(ns slackcommands.slack.events
  (:require [clojure.java.io :as io]
            [clj-http.client :as client]
            [clj-slack.chat :as chat]
            [slackcommands.util :as util]
            [clojure.core.async :as async]
            [clojure.string :as str]
            [slackcommands.ai.assistant :as assistant]
            [slackcommands.slack :as slack]
            [clojure.data.json :as json]))

(defn strip-prefix [prompt]
  (-> prompt 
      (str/replace #"^@U01729B7HC5[ ]*" "")
      (str/trim)) )

(defonce sent-msg-ids (atom #{}))
(defn events-api [req]
  ;; (clojure.pprint/pprint req)
  (let [js (with-open [body (:body req)
                       rdr (io/reader body)]
             (json/read rdr))
        event (get js "event")
        thread-ts (get event "thread_ts")
        
        channel (get event "channel")
        text (slack/blocks->text (get event "blocks"))

        thread-id (if thread-ts
                    thread-ts
                    (get event "ts"))

        files (get event "files")
        attachments
        (when (seq files)
          (into []
                (map
                 (fn [f]
                   {:mimetype (get f "mimetype")
                    :id (get f "id")
                    :url (delay
                           (let [url (get f "url_private")
                                 response (client/get 
                                           url
                                           {:headers {"Authorization" (str "Bearer " slack/slack-oauth-token)}
                                            :as :stream})
                                 fname (str (random-uuid) (util/content-type->suffix (get f "mimetype")))
                                 _ (prn (get f "mimetype") fname fname)
                                 public-url (util/save-and-upload-stream fname (:body response))]
                             public-url))}))
                files))

        ch (async/chan)
        text (strip-prefix text)
        audio-prompt? (and (empty? text)
                           (some #(util/audio? (:mimetype %)) attachments))
        text (if audio-prompt?
               "Transcribe the attached audio and follow the instructions."
               text)]
    ;; (clojure.pprint/pprint js)
    (async/thread
      (prn "responding to" ch thread-id text)
      (assistant/respond ch thread-id text attachments))
    (async/thread
      (let [placeholder-message
            (chat/post-message slack/conn
                               channel
                               ":thonking:"
                               {"thread_ts" thread-id})]
        (loop [first? true]
          (let [{:keys [id text] :as msg} (async/<!! ch)]
            (when (and first?
                       (:ts placeholder-message))
              (chat/delete slack/conn
                           (:ts placeholder-message)
                           channel))
            (prn "got msg" msg)
            (when msg
              (let [[old _] (swap-vals! sent-msg-ids conj id)]
                (when (not (contains? old id))
                  (doseq [chunk (partition-all 2999 text)
                          :let [subresponse (apply str chunk)]]
                    (chat/post-message slack/conn channel
                                       subresponse
                                       {"thread_ts" thread-id
                                        "blocks" (json/write-str
                                                  [{"type" "section"
                                                    "text" {"type" "mrkdwn"
                                                            "text" subresponse}}])})))
                (recur false)))))))

    {;; :body (get js "challenge")
     :headers {"Content-type" "text/plain"}
     :status 200}))




