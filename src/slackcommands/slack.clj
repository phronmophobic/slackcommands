(ns slackcommands.slack
  (:require [clj-slack.core :refer [slack-request]]
            [clj-slack.conversations :as conversations]
            [clj-slack.emoji :as emoji]
            [clj-slack.users :as users]
            [clj-slack.files :as files]
            [clojure.java.io :as io]
            [clj-slack.chat :as chat]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def slack-oauth-token
  (:slack/oauth-token
   (edn/read-string (slurp "secrets.edn"))))

(def conn 
  {:api-url "https://slack.com/api" :token slack-oauth-token})

(defn user-info*
  "Interesting keys
  :profile {:display_name}
  "
  [uid]
  (:user (users/info conn uid)))

(let [f (memoize user-info*)]
  (defn user-info
    "Interesting keys
  :profile {:display_name}
  "
    [uid]
    (f uid)))

(defn username [uid]
  (let [uinfo (user-info uid)
        profile (:profile uinfo)
        name (:display_name profile)]
    (if (seq name)
      name
      (:real_name profile))))

(defmulti block->text #(get % "type"))
(defmethod block->text "rich_text" [block]
  (str/join
   (eduction
    (map block->text)
    (get block "elements"))))

(defmethod block->text "rich_text_section" [block]
  (str/join
   (eduction
    (map block->text)
    (get block "elements"))))

(defmethod block->text "rich_text_quote" [block]
  (let [txt (str/join
             (eduction
              (map block->text)
              (get block "elements")))]
    (str/join
     "\n"
     (eduction
      (map #(str "> " %))
      (str/split-lines txt)))))


(defmethod block->text "context" [block]
  (str/join
   (eduction
    (map block->text)
    (get block "elements"))))

(defmethod block->text "rich_text_preformatted" [block]
  (str/join
   (eduction
    (map block->text)
    (get block "elements"))))

(defmethod block->text "user" [block]
  (str "@" (username (get block "user_id"))))

(def slack-message-regex
  #"https://realmonsters.slack.com/archives/([^/]+)/p([0-9]{10})([0-9]+)(\?.*)?")

(defmethod block->text "link" [block]
  (let [url (get block "url")]
    (if-let [[url channel-id ts1 ts2] (re-matches slack-message-regex url)]
      (str "thread-" channel-id "-" ts1 "." ts2 )
      (get block "url"))))

(defmethod block->text "text" [block]
  (get block "text"))

(defmethod block->text "channel" [block]
  (get block "channel_id"))

(defmethod block->text nil [block]
  (prn block)
  "")

(defmethod block->text "mrkdwn" [block]
  (get block "text"))

(defmethod block->text "section" [block]
  (block->text (get block "text")))

(defmethod block->text "input" [block]
  "")

(defmethod block->text "emoji" [block]
  (str ":"(get block "name") ":"))

(defmethod block->text "plain_text" [block]
  (get block "text"))

(defmethod block->text "image" [block]
  (get block "image_url"))

(defmethod block->text "rich_text_list" [block]
  (let [sep-xform (if (= (get block "style")
                         "ordered")
                    (map-indexed (fn [i s]
                                   (str (inc i) ". " s)))
                    (map (fn [s]
                           (str "- " s))))]
    (str
     (str/join
      "\n"
      (eduction
       (map block->text)
       sep-xform
       (get block "elements")))
     "\n")))

(defmethod block->text "divider" [block]
  "\n---------------------\n")

(defmethod block->text "actions" [block]
  "")

(defn blocks->text [blocks]
  ;; (clojure.pprint/pprint blocks)
  (try
    (str/join
     "\n"
     (eduction
      (map block->text)
      blocks))
    (catch Exception e
      (clojure.pprint/pprint blocks)
      (clojure.pprint/pprint e)
      (throw e))))



(defn retrieve-thread [channel-id thread-id]
  (let [replies (conversations/replies (assoc conn
                                              :key-fn identity)
                                       channel-id
                                       thread-id)]
    (when (get replies "ok")
      (str/join
       "\n---------------\n"
       (eduction
        (map (fn [msg]
               (str "@" (username (get msg "user")) ": \n"
                    (blocks->text (get msg "blocks")))))
        (get replies "messages"))))))

(defn thread-attachments [channel-id thread-id]
  (let [response (conversations/replies
                  conn
                  channel-id
                  thread-id)
        messages (:messages response)
        files (->> messages
                   (mapcat :files)
                   (map (fn [finfo]
                          {:url (:url_private finfo)
                           :name (:name finfo)
                           :mimetype (:mimetype finfo)})))
        attachments (->> messages
                         (mapcat :attachments)
                         (keep (fn [info]
                                 (when-let [url (:image_url info)]
                                   {:url url
                                    :name (:fallback info)}))))]
    (concat
     files
     attachments)))

(defn message-update [connection channel-id timestamp opts]
  (slack-request connection "chat.update" 
                 (merge {"ts" timestamp "channel" channel-id}
                        opts)))

(defn get-channel-id [channel-name]
  (->> (conversations/list conn 
                           {"types" "public_channel"})
       :channels
       (filter #(= channel-name
                   (:name %)))
       first
       :id))



(defn list-emoji []
  (:emoji (emoji/list conn)))

(def main-channel-id
  (delay
    (get-channel-id "shaitposting")))

(defn send-to-main [mdown]
  (chat/post-message
   conn
   @main-channel-id
   mdown
   {"blocks" (json/write-str
              [{"type" "section"
                "text" {"type" "mrkdwn"
                        "text" mdown}}])}))


(def test-channel-id
  (delay (get-channel-id "test")))

(defn send-to-test [mdown]
  (chat/post-message
   conn
   @test-channel-id
   mdown
   {"blocks" (json/write-str
              [{"type" "section"
                "text" {"type" "mrkdwn"
                        "text" mdown}}])}))

(defn read-thread [channel-id thread-id]
  (str/join
   (eduction
    
    (:messages
     (conversations/replies conn channel-id thread-id))))
  )

(defn delete-message [channel-id thread-id]
  (chat/delete conn thread-id channel-id))

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

  
  (chat/post-message
   slack/conn
   channel-id 
   ":waiting-cat:")

  

  
,)


