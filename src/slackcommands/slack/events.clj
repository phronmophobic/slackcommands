(ns slackcommands.slack.events
  (:require [clojure.java.io :as io]
            [clj-http.client :as client]
            [clj-slack.chat :as chat]
            [slackcommands.util :as util]
            [clojure.core.async :as async]
            [clojure.string :as str]
            [slackcommands.ai.assistant2 :as assistant]
            [slackcommands.slack :as slack]
            [clojure.data.json :as json]))

(defn strip-prefix [prompt]
  (-> prompt 
      (str/replace #"^@realmonsters[ ]*" "")
      (str/trim)) )

(def my-id "U01729B7HC5")

(defonce sent-msg-ids (atom #{}))

(defn format-response [text]
  (try
    (let [matches (str/replace text
                               #"\@U[A-Za-z0-9]+"
                               (fn [match]
                                 (let [ ;; chop off @
                                       uid (subs match 1)
                                       username (slack/username uid)]
                                   (if (seq username)
                                     username
                                     match))))]
      matches)
    (catch Exception e
      (prn "Exception formatting response!")
      text)))

(def SLACK-TIMEOUT 15e3)
(def SLACK-MAX-LENGTH 2999)
(defn handle-event [event]
  (let [thread-ts (get event "thread_ts")
        
        channel (get event "channel")
        text (slack/blocks->text (get event "blocks"))

        thread-id (if thread-ts
                    thread-ts
                    (get event "ts"))

        user-id (get event "user")
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

        ch (async/chan 5)
        ;; status-ch (async/chan)
        text (strip-prefix text)
        placeholder-snippet (if (< (count text) 60)
                              text
                              (str (subs text 0 60) "..."))

        audio-prompt? (and (empty? text)
                           (some #(util/audio? (:mimetype %)) attachments))
        text (if audio-prompt?
               "Transcribe the attached audio and follow the instructions."
               text)]
    ;; (clojure.pprint/pprint event)
    (assistant/respond
     {:ch ch
      :slack/thread-id thread-id
      :slack/channel channel
      :slack/new-thread? (not thread-ts)
      :slack/user-id user-id
      :slack/username (slack/username user-id)
      :prompt text
      :attachments attachments})

    (async/thread
      (try
        (let [message (chat/post-message slack/conn
                                         channel
                                         (str ":thonking: "
                                              placeholder-snippet)
                                         {"thread_ts" thread-id})
              update-message (fn [[ts offset] markdown]
                               (loop [ts ts
                                      offset offset]
                                 (let [chunk (subs markdown offset (min (count markdown)
                                                                        (+ offset SLACK-MAX-LENGTH)))
                                       {:keys [ts]} (if ts
                                                      (slack/message-update
                                                       slack/conn
                                                       channel
                                                       ts
                                                       {"blocks"
                                                        (json/write-str
                                                         [{"type" "section"
                                                           "text" {"type" "mrkdwn"
                                                                   "text" chunk}}])})
                                                      (chat/post-message slack/conn
                                                                         channel
                                                                         chunk
                                                                         {"thread_ts" thread-id
                                                                          "blocks"
                                                                          (json/write-str
                                                                           [{"type" "section"
                                                                             "text" {"type" "mrkdwn"
                                                                                     "text" chunk}}])}))]
                                   (if (< (count markdown) (+ offset SLACK-MAX-LENGTH))
                                     [ts offset]
                                     (recur nil (+ offset SLACK-MAX-LENGTH))))))]

          (when (:ts message)
            (loop [cursor [(:ts message) 0] 
                   timeout (async/timeout SLACK-TIMEOUT)
                   latest-content ""]
              (async/alt!!
                timeout ([_]
                         (recur (if (seq latest-content)
                                  (update-message cursor
                                                  latest-content)
                                  cursor)
                                (async/timeout SLACK-TIMEOUT)
                                latest-content))
                ch ([chunk]
                    (if chunk
                      (cond
                        (instance? Exception chunk)
                        (update-message cursor "uhoh.")

                        (:tool_calls chunk)
                        (let [tool-calls
                              (str "tool calls: "
                                   (str/join
                                    ", "
                                    (eduction
                                     (map :function)
                                     (map :name)
                                     (:tool_calls chunk))))]
                          (if (= latest-content tool-calls)
                            (recur cursor timeout latest-content)
                            (recur
                             (update-message cursor tool-calls)
                             timeout
                             tool-calls)))

                        (:content chunk)
                        (recur cursor timeout (:content chunk))

                        :else
                        (recur cursor timeout latest-content))

                      ;; done
                      (do
                        (update-message cursor latest-content))))))))
        (catch Exception e
          (prn "error!")
          (clojure.pprint/pprint e))
        (finally 
          ;;(prn "slack thread stopped")
          )))))

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
                         (or (= "file_share" (get event "subtype"))
                             (not (get event "subtype")))
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

