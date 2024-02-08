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

(def my-id "U01729B7HC5")

(defonce sent-msg-ids (atom #{}))

(defn handle-event [event]
  (let [thread-ts (get event "thread_ts")
        
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
        status-ch (async/chan)
        text (strip-prefix text)
        audio-prompt? (and (empty? text)
                           (some #(util/audio? (:mimetype %)) attachments))
        text (if audio-prompt?
               "Transcribe the attached audio and follow the instructions."
               text)]
    ;; (clojure.pprint/pprint js)
    (async/thread
      (prn "responding to" ch thread-id text)
      (assistant/respond
       {:ch ch
        :slack/thread-id thread-id
        :slack/channel channel
        :prompt text
        :attachments attachments
        :status-ch status-ch}))

    (async/thread
      (let [placeholder-message
            (chat/post-message slack/conn
                               channel
                               ":thonking:"
                               {"thread_ts" thread-id})]
        (when (:ts placeholder-message)
          (async/thread
            (loop []
              (let [msg (async/<!! status-ch)]
                (when msg
                  (slack/message-update slack/conn
                                        channel
                                        (:ts placeholder-message)
                                        {"blocks"
                                         (json/write-str
                                          [{"type" "section"
                                            "text" {"type" "mrkdwn"
                                                    "text" (str ":thonking: " msg)}}])})
                  (recur))))))

        (loop [first? true]
          (let [{:keys [id text] :as msg} (async/<!! ch)]
            (when (and first?
                       (:ts placeholder-message))
              (async/close! status-ch)
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
                (recur false)))))))))

(defn events-api [req]
  ;; (clojure.pprint/pprint req)
  #_(let [js (with-open [body (:body req)
                       rdr (io/reader body)]
             (json/read rdr))]
    {:body (get js "challenge")
     :headers {"Content-type" "text/plain"}
     :status 200})

  (let [js (with-open [body (:body req)
                       rdr (io/reader body)]
             (json/read rdr))
        event (get js "event")]
    (if (= "app_mention"
              (get event "type"))
      (handle-event event)
      ;; else
      (let [text (slack/blocks->text (get event "blocks"))
            handle? (and (not= (get event "user")
                               my-id)
                         (= "message" (get event "type"))
                         (not (get event "subtype"))
                         (not (str/starts-with? text "/")))]
        ;; (prn "handle?" handle?)
        ;; (clojure.pprint/pprint event)
        (when handle?
          (handle-event event) )))

    {;; :body (get js "challenge")
     :headers {"Content-type" "text/plain"}
     :status 200}))


(comment

  (chat/post-message slack/conn
                     @slack/main-channel-id
                     "Here you go!"
                     {"thread_ts" "..."})
  ,)

