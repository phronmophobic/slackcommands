(ns slackcommands.ai.assistant2
  (:require [wkok.openai-clojure.api :as openai]
            [clojure.java.shell :as sh]
            [clojure.edn :as edn]
            [com.phronemophobic.discord.api :as discord]
            [com.phronemophobic.clj-media :as clj-media]
            [clojure.core.async :as async]
            [clj-http.client :as http]
            [slingshot.slingshot :refer [throw+ try+]]
            [clojure.data.json :as json]
            [com.phronemophobic.discord.api :as discord]
            [com.phronemophobic.nubes :as nubes]
            [com.phronemophobic.alpaca :as alpaca]
            [slackcommands.util :as util]
            [slackcommands.util.audio :as util.audio]
            [slackcommands.gif :as gif]
            [slackcommands.ai.img :as img]
            [slackcommands.slack :as slack]
            [slackcommands.ai.vision :as vision]
            [slackcommands.emoji :as emoji]
            [slackcommands.db :as db]
            [slackcommands.stability :as stability]
            [slackcommands.yt-dlp :as yt-dlp]
            [slackcommands.flux :as flux]
            [slackcommands.perplexity :as perplexity]
            [amazonica.aws.s3 :as s3]
            [clj-slack.chat :as chat]
            [membrane.ui :as ui]
            [datalevin.core :as d]
            [pantomime.mime :as mime]
            pantomime.media
            pantomime.web
            pantomime.extract
            [clojure.java.io :as io]
            [clj-http.client :as client]
            [clojure.string :as str])
  (:import java.util.concurrent.Executors
           java.time.LocalDate
           java.time.format.DateTimeFormatter
           java.util.Date))

(declare respond)

(def openai-key
  (:chatgpt/api-key
   (edn/read-string (slurp "secrets.edn"))))

(def thread-table "thread-table")
(defonce db
  (delay
    (doto (d/open-kv (.getCanonicalPath (io/file "threadkv.db")))
      (d/open-dbi thread-table))))

;; image segmentation
;; https://huggingface.co/facebook/sam-vit-base

(defn sanitize-name [s]
  (str/replace s #"[^A-Za-z0-9]" "-"))

(defonce tool-executor
  (delay
    (let [thread-factory
          (reify
            java.util.concurrent.ThreadFactory
            (newThread [this r]
              (let [thread (.newThread (Executors/defaultThreadFactory)
                                       r)]
                ;; set priority to two less than normal
                (.setPriority thread
                              (max Thread/MIN_PRIORITY
                                   (dec
                                    (dec Thread/NORM_PRIORITY))))
                thread)))]
      (Executors/newCachedThreadPool thread-factory))))

(comment
  ;; alloy, echo, fable, onyx, nova, and shimmer
  (def voice "alloy")
  (def speech
    (openai/create-speech {:model "tts-1"
                          :input "hello"
                          :voice voice
                          :response_format "mp3"}
                          {:api-key openai-key}))
,)

(defn truncate [s n]
  (subs s 0 (min (count s)
                 n)))

(defn prn-truncate
  ([o]
   (prn-truncate o 5000))
  ([o n]
   (println
    (truncate (pr-str o)
              n))))

(defonce wrapped-callbacks
  (atom {}))
(defn callback-wrapper [payload {:keys [id payload?]}]
  (future
    (let [cb (get @wrapped-callbacks id)]
      (try
        (if payload?
          (cb payload)
          (cb))
        (catch Exception e
          (prn-truncate e)))))
  (util/delete-message payload))
(defn wrap-callback
  ([f]
   (wrap-callback f false))
  ([f payload?]
   (let [callback-id (str (random-uuid))]
     (swap! wrapped-callbacks assoc callback-id f)
     (util/make-action `callback-wrapper {:id callback-id
                                          :payload? payload?}))))
(defn prompt
  "button-actions is vector of:
  {:text \"Approve\"
   :style one of :default, :primary, or :danger
   :action should be result of make-action.
  }  "
  ([channel request-markdown button-actions]
   (prompt channel nil request-markdown button-actions ))
  ([channel thread-id request-markdown button-actions]
   (let [blocks [
                 {
                  "type" "section",
                  "text" {
	                  "type" "mrkdwn",
	                  "text" request-markdown
	                  }
                  },
                 {
                  "type" "actions",
                  "elements" 
                  (into []
                        (map (fn [action]
                               (merge
                                {"type" "button",
		                 "text" {
			                 "type" "plain_text",
			                 "emoji" true,
			                 "text" (:text action)
			                 },
		                 
		                 "value" (:action action)}
                                (case (:style action)
                                  (nil :default) {}
                                  :primary {"style" "primary"}
                                  :danger {"style" "danger"}))))
                        button-actions)}]]
     (chat/post-message slack/conn
                        channel
                        nil
                        (merge
                         {"blocks" blocks}
                         (when thread-id
                           { "thread_ts" thread-id}))))))
(defn get-approval [channel thread-id markdown]
  (let [p (promise)
        prompt-message
        (prompt channel thread-id markdown
                [{:text "Approve"
                  :style :primary
                  :action (wrap-callback 
                           (fn []
                             (deliver p true)))}
                 {:text "Deny"
                  :style :danger
                  :action (wrap-callback 
                           (fn []
                             (deliver p false)))}])]
    ;; timeout after 5 minutes
    (let [result (deref p (* 1000 60 5) ::timeout)]
      (if (= ::timeout result)
        (do
          (future
            (chat/delete slack/conn
                         (:ts prompt-message)
                         (:channel prompt-message)))
          false)
        result))))

(comment
  (prompt @slack/test-channel-id
          "Do you want to do the thing?"
          [{:text "yes"
            :action (util/make-action 'foo {})}])
  ,)

(defn text-to-speech [{:strs [text voice]}]
  (let [voice (or voice "alloy")
        is (openai/create-speech {:model "tts-1"
                                  :input text
                                  :voice voice
                                  :response_format "mp3"}
                                 {:api-key openai-key})
        fname (str (sanitize-name (truncate text 15)) ".mp3")
        url (util/save-and-upload-stream fname is)]
    url))

(defn transcribe [{:strs [url]}]
  (let [
        text (util.audio/transcribe-url url)
        ;; todo: inline text response if short
        url (util/save-and-upload-stream
             (str "transcription-" (random-uuid) ".txt")
             (java.io.ByteArrayInputStream.
              (.getBytes text "utf-8")))]
    (str "The transcription can be found at " url)))

(comment
  (def myresult
(let [response (http/get "https://www.allrecipes.com/gingerbear-thumbprint-cookies-recipe-8386095")
         html (:body response)
         readability-cli (.getCanonicalPath
                          (io/file ".."
                                   "readability/node_modules/.bin/readable"))
         args ["--json"
               "-"
               :in (.getBytes html "utf-8")]
      {:keys [out]} (apply sh/sh readability-cli args)]
  (json/read-str out)
     ))
  ,)

(defn link-reader [{:strs [url]}]
  (let [url (util/maybe-download-slack-url url)
        response (http/get url
                           {:as :stream})
        content-type (get-in response [:headers "Content-Type"])]
    (case content-type
      "application/pdf"
      (-> (:body response)
          pantomime.extract/parse
          :text)
      ;; should use pantomime
      #_#_(;; "image/jpeg" "image/png"
       "application/pdf")
      (let [suffix (case content-type
                     "image/jpeg" "jpg"
                     "image/png" "png"
                     "application/pdf" "pdf")
            f (util/stream->file (str (random-uuid) "." suffix)
                                 (:body response))
            aifile (openai/create-file 
                    {:purpose "assistants"
                     :file f}
                    {:api-key openai-key})
            ;; _ (openai/modify-assistant
            ;;    {:name "Scruffy"
            ;;     :assistant_id assistant-id
            ;;     :model "gpt-4-1106-preview"
            ;;     :tools (into [{:type "code_interpreter"}
            ;;                   ;;{:type "retrieval"}
            ;;                   ]
            ;;                  tools)
            ;;     :instructions "Your a helpful assistant that is secretly an extremely smart dog. You try to avoid revealing that you are dog, but sometimes your dogginess shows when you get excited. If someone asks if you a dog, pretend you didn't hear them or ignore the question."}
            ;;    {:api-key openai-key})
            assistant-file (openai/create-assistant-file 
                            {:assistant_id assistant-id
                             :file_id (:id aifile)}
                            {:api-key openai-key})]
        (:id aifile))

      ;; else assume htmlish
      (let [html (slurp (:body response)
                        :encoding "utf-8")
            readability-cli (.getCanonicalPath
                             (io/file ".."
                                      "readability/node_modules/.bin/readable"))
            args ["--json"
                  "-"
                  :in (.getBytes html "utf-8")]
            {:keys [out]} (apply sh/sh readability-cli args)
            {:strs [title
                    byline
                    text-content]} (if (= "" out)
                                     {"text-content" html}
                                     (json/read-str out))]
        (clojure.string/join
         "\n"
         (eduction
          (filter some?)
          [title
           (str "By " byline)
           text-content]))))))




(defn generate-image [{:strs [prompt
                              using
                              urls]}]
  (let [prompt (if (and (seq urls)
                        (not= "dalle" using))
                 (str (str/join " "
                                (map util/maybe-download-slack-url urls))
                      " "
                      prompt)
                 prompt)]
    (case using
      "dalle"
      (let [response (openai/create-image {:prompt prompt
                                           :n 1
                                           :model "dall-e-3"
                                           :size
                                           ;; "256x256"
                                           ;; "512x512"
                                           "1024x1024"
                                           }
                                          {:api-key openai-key})
            url (->> (:data response)
                     first
                     :url
                     (util/save-and-upload-large-png))]
        url)

      "pixel-art-xl"
      (let [paths (nubes/generate-pixel-art prompt)]
        (str/join "\n"
                  (eduction
                   (map (fn [path]
                          (util/save-and-upload-large-png 
                           (io/file path))))
                   (map-indexed (fn [i url]
                                  (str i ". " url)))
                   paths)))

      ("flux-pro"
       "flux-schnell"
       "flux-dev"
       "flux")
      (let [url (flux/generate-image {:model (keyword using)
                                      :prompt prompt})]
        url)

      ;; else
      (let [response (discord/create-image prompt)]
        (if-let [url (:url response)]
          (let [img-urls (util/split4-large-png url)]
            (str/join "\n"
                      (eduction
                       (map-indexed (fn [i url]
                                      (str i ". " url)))
                       img-urls)))
          (throw (ex-info "Error" response)))))))

(defn create-illusion [{:strs [prompt image_url]}]
  (let [url (flux/generate-image {:model :illusion-diffusion
                                  :image-url image_url
                                  :prompt prompt})]
    url))


(defn generate-3d-model [{:strs [image_url] :as m}]
  (prn "input" m)
  (let [url (stability/stable3d {:image-url image_url})]
    (str "Here is the model: " url ".")))

(defn image-edit-with-replacement [{:strs [image_url prompt search_prompt]}]
  (let [url (stability/search-and-replace {:image-url image_url
                                           :prompt prompt
                                           :search-prompt search_prompt})]
    (str "Here is the edited image: " url ".")))

(defonce feature-request-lock (Object.))
(defn feature-request [{:strs [feature_description]}]
  (locking feature-request-lock 
    (spit "feature-requests.txt" feature_description :append true)
    (spit "feature-requests.txt" "\n---------------------------\n" :append true))
  "Thank you for your feature request. It has been logged.")


(defn publish-html [{:strs [html]}]
  (util/save-and-upload-stream
   (str (random-uuid) ".html")
   (java.io.ByteArrayInputStream. 
    (.getBytes html "utf-8"))))

(defn attachment-content [{:strs [thread_id]
                           :keys [slack/channel slack/thread-id]}]
  (let [[channel-id thread-id] (if thread_id
                                 (let [[_ channel-id thread-id] (str/split thread_id #"-")]
                                   [channel-id thread-id])
                                 [channel thread-id])
        thread-attachments (slack/thread-attachments channel-id thread-id)]
    (if (seq thread-attachments)
      (into [{:type "text"
              :text "Here are the attachments:\n"}]
            (comp
             (mapcat (fn [{:keys [url name mimetype]}]
                       (let [url (util/maybe-download-slack-url url)
                             messages [{:type "text"
                                        :text url}]
                             messages (if (or (and mimetype
                                                   (pantomime.media/image? mimetype))
                                              (and url
                                                   (pantomime.media/image?
                                                    (mime/mime-type-of url))))
                                        (conj messages {:type "image_url"
                                                        :image_url {:url url}})
                                        messages)]
                         messages)))
             (interpose {:type "text"
                         :text "\n"}))
            thread-attachments)
      ;; no attachments
      nil)))

(defn list-attachments [{:keys [slack/channel
                                slack/thread-id]
                         tool-call-id :tool-call/id
                         :as args}]
  (let [attachment-content (attachment-content args)]
    (if (seq attachment-content)
      {::messages [{:tool_call_id tool-call-id
                    :role "tool"
                    :content "The attachments will be listed."}
                   {:role "user"
                    :content attachment-content}]}
      "No attachments were found.")))

(defn sanitize-url-name [s]
  (-> s
      (truncate 32)
      (str/replace #"[^A-Za-z0-9-_.]" "-")))


(defn upload-url [{:strs [url path]}]
  (let [fname (if path
                (sanitize-url-name path)
                (let [mt (mime/mime-type-of url)
                      ext (mime/extension-for-name mt)]
                  (str (random-uuid)
                       ext)))
        url (util/maybe-download-slack-url url)
        ;; prefix with u/ for all user generated urls.
        key (str "u/" fname)
        
        uploaded-url (str "https://" "aimages.smith.rocks/" key)

        ;; must download file for put-object
        ;; it accepts input streams, but not
        ;; the kind opened with io/as-url
        ;; also, this gives a check on file sizes uploaded
        ;; files that are too large will fill up the local disk and break.
        f (util/url->file fname url)]
    (s3/put-object util/bucket
                   key
                   f)
    (str "The url has been uploaded to: " uploaded-url)))

(defn download-video [{:strs [url]}]
  (yt-dlp/download-vid url))

(defn ingest-url [{:strs [url]
                   :as args}]
  (let [request {:as :stream}
        request (if (str/starts-with? url "https://files.slack.com/")
                  (assoc request :headers {"Authorization" (str "Bearer " slack/slack-oauth-token)})
                  request)
        response (client/get url request)


        temp-file (util/temp-file "ingest"
                                  "")]
    (with-open [os (io/output-stream temp-file)]
      (io/copy (:body response)
               os))
    (let [mime-type (mime/mime-type-of temp-file)
          suffix (mime/extension-for-name mime-type)

          temp-file2 (util/temp-file "ingest" suffix)
          _ (.renameTo temp-file temp-file2)
          
          purpose (if (pantomime.media/image? mime-type)
                    "vision"
                    "assistants")
          {file-id :id} (openai/create-file {:purpose purpose
                                             :file temp-file2}
                                            {:api-key openai-key})
          _ (.delete temp-file)
          _ (.delete temp-file2)]

      (when-let [prompt-ch (::prompt-ch args)]
        (async/put! prompt-ch
                    (merge
                     (select-keys args
                                  [:slack/thread-id
                                   :slack/channel])
                     {:ch (async/chan (async/dropping-buffer 1))
                      :status-ch (async/chan (async/dropping-buffer 1))
                      :assistant/message {:thread_id (:assistant/thread-id args)
                                          :role "assistant"
                                          :attachments [{:file_id file-id
                                                         :tools [{"type" "code_interpreter"}]}]
                                          :content (str "Attaching " file-id)}})))
      
      (str "The file-id of the ingested file will be " file-id))))

(defn load-images [{:strs [image_urls]
                    tool-call-id :tool-call/id}]
  {::messages [{:tool_call_id tool-call-id
                :role "tool"
                :content "The load has completed."}
               {:role "user"
                :content
                (into []
                      (comp (map util/maybe-download-slack-url)
                            (filter (fn [url]
                                      (pantomime.media/image?
                                       (mime/mime-type-of url))))
                            (map (fn [url]
                                   {:type "image_url"
                                    :image_url {:url url}})))
                      image_urls)}]})


(defn send-to-main [{:strs [markdown]
                     :keys [slack/channel
                            slack/thread-id]}]
  ;; (slack/send-to-main markdown)
  (let [p (promise)]
    (prompt channel thread-id ":this-is-fine-but-with-ai: would like to send a message to main."
            [{:text "Approve"
              :style :primary
              :action (wrap-callback 
                       (fn []
                         (deliver p true)))}
             {:text "Deny"
              :style :danger
              :action (wrap-callback 
                       (fn []
                         (deliver p false)))}])
    (let [approved? @p]
      (if approved?
        (do
          (chat/post-message slack/conn channel
                             markdown
                             {"blocks" (json/write-str
                                        [{"type" "section"
                                          "text" {"type" "mrkdwn"
                                                  "text" markdown}}])})
          "Message sent.")
        "Your request to send to main was denied."))))

(defn send-to-channel [{:strs [markdown channel_id]
                        :keys [slack/channel
                               slack/thread-id]}]
  ;; (slack/send-to-main markdown)
  (let [p (promise)]
    (prompt channel thread-id ":this-is-fine-but-with-ai: would like to send a message."
            [{:text "Approve"
              :style :primary
              :action (wrap-callback
                       (fn []
                         (deliver p true)))}
             {:text "Deny"
              :style :danger
              :action (wrap-callback
                       (fn []
                         (deliver p false)))}])
    (let [approved? @p]
      (if approved?
        (do
          (chat/post-message slack/conn channel_id
                             markdown
                             {"blocks" [{"type" "section"
                                         "text" {"type" "mrkdwn"
                                                 "text" markdown}}]})
         "Message sent.")
        "Your request was denied."))))


(defn retrieve-thread [{:strs [thread_id]
                        :keys [slack/channel slack/thread-id]
                        tool-call-id :tool-call/id
                        :as args}]
  (let [[channel-id thread-id]
        (if thread_id
          (let [[_ channel-id thread-id] (str/split thread_id #"-")]
            [channel-id thread-id])
          [channel thread-id])]
    (if-let [s (slack/retrieve-thread channel-id thread-id)]
      (let [attachment-content (attachment-content args)]
        (if (seq attachment-content)
          {::messages [{:tool_call_id tool-call-id
                        :role "tool"
                        :content (str "The attachments will be listed. Below is a transcript of the thread.\n\n"
                                      s)}
                       {:role "user"
                        :content attachment-content}]}
          (str "Below is a transcript of the thread.\n\n"
               s)))
      "No thread found for that thread id.")))

(defn delete-message [{:strs [thread_id]}]
  (let [[_ channel-id thread-id] (str/split thread_id #"-")
        thread-id (if (str/starts-with? thread-id "p")
                    (subs thread-id 1)
                    thread-id)
        {:keys [ok error]} (slack/delete-message channel-id thread-id)]
    (if ok
      "Done!"
      (do
        (prn "there was a delete error: " error)
        "There was an error."))))

(defn examine-image [{:strs [url]
                      :keys [slack/channel slack/thread-id]}]
  (let [url (util/maybe-download-slack-url url)
        objs (vision/find-objects url)]
    (if (seq objs)
      (let [img-url (util/save-and-upload-view
                     #(ui/padding
                       10
                       (vision/highlight-objects url objs)))
            [w h] (ui/bounds (ui/image (io/as-url url)))]
        (chat/post-message slack/conn
                           channel
                           nil
                           {"thread_ts" thread-id
                            "blocks" 
                            [{"type" "section"
                              "text" {"type" "mrkdwn"
                                      "text" img-url}}]})
        (with-out-str
          (clojure.pprint/print-table
           ["name" "score" "id" "x" "y" "width" "height"]
           (eduction
            (map (fn [{:keys [name score mid bounding-poly]}]
                   (let [minx (apply min (map :x bounding-poly))
                         maxx (apply max (map :x bounding-poly))
                         miny (apply min (map :y bounding-poly))
                         maxy (apply max (map :y bounding-poly))]
                     {"name" name
                      "score" score
                      "x" (long (* w minx))
                      "y" (long (* h miny))
                      "width" (long (* w (- maxx minx)))
                      "height" (long (* h (- maxy miny)))
                      "id" mid})))
            objs))))
      "No objects found.")))

(defn computer-enhance-image [{:strs [image_url model]}]
  (let [url (flux/upscale {:image-url image_url
                           :model (keyword model)})]
      (str "Here is the enhanced image:" url))
  #_(let [url (nubes/enhance (util/maybe-download-slack-url image_url))]
    (str "Here is the enhanced image:" url)))

(defn slackify-gif [{:strs [url]}]
  (let [url (gif/shrink-gif (util/maybe-download-slack-url url))]
    (str "Here is the slackified gif:" url)))

(defn emoji-image-url [{:strs [emoji]}]
  (emoji/emoji->url (str/replace emoji #":" "")))

(defn rustle-image [{:strs [image_url emoji max max_offset direction]
                     :as m}]
  (let [opts {:alpha-threshold (get m "alpha_threshold" 128)
              :transparent? (get m "transparent" true)
              ;; :crop? (get m "crop" true)
              :max-offset (get m "max_offset" 4)
              :direction (get m "direction" "random")
              :max? max
              :fps (get m "fps" 24)}
        image-url (if image_url
                    (util/maybe-download-slack-url image_url)
                    (let [emoji (str/replace emoji #":" "")]
                      (emoji/emoji->url emoji)))]
    (if image-url
      (let [url-cropped (gif/rustle-image image-url (assoc opts :crop? true))
            url-uncropped (gif/rustle-image image-url (assoc opts :crop? false))]
        (str "Here are the rustled images:\n" 
             "cropped: " url-cropped "\n"
             "without cropping: "url-uncropped "\n"))
      "An image_url or emoji must be provided.")))

(defn scrollgif [{:strs [text]
                     :as m}]
  (let [url (gif/scrollgif {:text text})]
    (str "Here is the scrollgif: " url)))

(defn label-image [{:strs [url]}]
  (let [labels (vision/label-image (util/maybe-download-slack-url url))]
    (if (seq labels)
      (str
       "Please find the labels below:\n"
       (with-out-str
         (clojure.pprint/print-table
          ["description" "score" "topicality"]
          (eduction
           (map (fn [{:keys [description score topicality]}]
                  {"description" description
                   "score" score
                   "topicality" topicality}))
           labels))))
      "No labels found.")))

(defn extract-text [{:strs [url]}]
  (let [text (vision/extract-text (util/maybe-download-slack-url url))]
    (if (seq text)
      (if (str/includes? text "\"")
        (str "The extracted text:\n" text)
        (str "The extracted text is \"" text "\"."))
      "No text found.")))

(defn run-llava [{:strs [prompt image_url]}]
  (nubes/run-llava prompt (util/maybe-download-slack-url image_url)))

(defn search-the-internet [{:strs [prompt]}]
  (let [response (perplexity/chat-completions
                  {:messages [{:role "system"
                               :content "Be precise and concise."}
                              {:role "user"
                               :content prompt}]})
        text (->> response
                  :choices
                  (map :message)
                  (filter #(= "assistant" (:role %)))
                  (map :content)
                  first)]
    (when-not text
      (throw (ex-info "No response for web search?"
                      {:prompt prompt
                       :response response})))
    text))


#_(defn resketch [{:strs [prompt image_url]}]
  (let [urls (nubes/generate-sketch prompt (util/maybe-download-slack-url image_url))]
    (str "Here are the images:\n"
         (str/join "\n"
                   (eduction
                    (map-indexed (fn [i url]
                                   (str i ". " url)))
                    urls)))))

(defn remove-image-background [{:strs [image_url]}]
  (let [url (nubes/remove-background (util/maybe-download-slack-url image_url))]
    (str "Here are the image without the background: " url)))

(defn generate-music [{:strs [prompt]}]
  (let [url (flux/generate-audio {:prompt prompt})]
    (str "Here is the music clip: " url))
  #_(let [paths (nubes/generate-music prompt)]
    (str "Here are the music clips:\n"
         (str/join "\n"
                   (eduction
                    (map-indexed (fn [i url]
                                   (str i ". " url)))
                    paths)))))


(defn animate [{:strs [image_url prompt model]}]
  (let [url (flux/animate {:image-url image_url
                           :model (keyword model)
                           :prompt prompt})]
    (str "Here is the animation: " url)
    )
  #_(let [image_urls (map util/maybe-download-slack-url image_urls)
        urls (nubes/stable-video-diffusion image_urls)]
      (str "Here are the animations:\n"
         (str/join "\n"
                   (eduction
                    (map-indexed (fn [i url]
                                   (str i ". " url)))
                    urls)))))


(defn talking-head [{:strs [image_url audio_url]}]
  (let [url (flux/sadtalker {:image-url image_url
                             :audio-url audio_url})]
    (str "Here is the animation: " url)))

(def treats
  {":bone:" "Virtual Bones: Crunchy and satisfying, they're the classic choice for any dog-inspired assistant's digital snack drawer."
   ":tennis:" "Squeaky Toys: Perfect for a quick play break between tasks, the squeak is irresistibly engaging."
   ":meat_on_bone:" "Meaty Snacks: Imaginary jerky or a virtual steak? Yes, please! They're ideal for a job well done."
   ":flying_disc:" "Frisbees: For those moments when you just need to stretch the ol' digital legs and leap through the air."
   ":feet:" "Paw-shaped Stickers: Cute and indicative of a \"pawsitive\" outcome!"
   ":bathtub:" "Bubbles: Who doesn't love chasing after shimmering bubbles, even if they're just on screen?"
   ":teddy_bear:" "Stuffed Animals: A fluffy companion to share in the successes of the day."
   ":deciduous_tree:" "A Tree to Sniff: Well, hypothetically."
   ":cheese_wedge:" "Cheese Chunks: A gourmet virtual snack for an especially complicated task."
   ":cookie:" "Doggy Cookies: Tailored for a digital palate, these come in an array of pixelated flavors."})

(defn log-treat [user-id treat-id treat-description]
  (db/transact! [{:event/time (Date.)
                  :event/type ::dispense-treat
                  :event/user user-id
                  :treat/id treat-id
                  :treat/description treat-description}]))

(def treat-stats-bg "https://aimages.smith.rocks/bdcd2d20-b94f-4b8a-b7fb-dd256e803bbd.jpg")
(defn shadowed-label [s]
  (let [s (str/join
           "\n"
           (eduction
            (map #(apply str %))
            (partition-all 70
                           (str s))))
        lbl (ui/label s)]
    [(ui/with-color [1 1 1]
       (ui/translate 1 1
                     lbl)
       (ui/translate -1 1
                     lbl)
       (ui/translate 1 -1
                     lbl)
       (ui/translate -1 -1
                     lbl))
     lbl]))

(defn treat-stats-ui* [stats]
  (let [
        by-user
        (apply
         ui/vertical-layout
         (for [[user count] (:by-user-counts stats)]
           (shadowed-label (str user ": " count))))
        by-treat
        (apply
         ui/vertical-layout
         (for [[treat count] (:by-treat-counts stats)]
           (let [treat-view (if (re-matches #"^:.*:$" treat)
                              (try
                                (ui/image
                                 (io/as-url (emoji/emoji->url (str/replace treat #":" "")))
                                 [25 nil])
                                (catch Exception e
                                  (let [data (ex-data e)]
                                    (if-let [emoji (:emoji data)]
                                      (shadowed-label emoji)
                                      (throw e)))))
                              (shadowed-label treat))]
             (ui/horizontal-layout
              treat-view
              (ui/spacer 10)
              (shadowed-label count)))))
        body (ui/padding 5
                         (ui/vertical-layout
                          by-user
                          by-treat))
        bg (ui/image (io/as-url treat-stats-bg)
                     [nil (ui/height body)])]
    [bg
     body]))

(defn treat-stats* []
  (let [dispenses (->> (db/q '[:find (pull ?e [*])
                               :where
                               [?e :event/type ::dispense-treat]])
                       (map first))
        by-user-counts (->> dispenses
                            (map :event/user)
                            (map slack/user-info)
                            (map :profile)
                            (map (fn [profile]
                                   (let [name (:display_name profile)]
                                     (if (seq name)
                                       name
                                       (:real_name profile)))))
                            frequencies)
        by-treat-counts (->> dispenses
                             (map (fn [event]
                                    (let [id (:treat/id event)]
                                      (if (= id "custom")
                                        (:treat/description event)
                                        id))))
                             frequencies)]
    {:by-user-counts by-user-counts
     :by-treat-counts by-treat-counts}))

(defn treat-stats [{:keys [slack/channel slack/thread-id]}]
  (let [stats (treat-stats*)]
    (chat/post-message slack/conn
                    channel
                    nil
                    {"thread_ts" thread-id
                     "blocks" 
                     [{"type" "section"
                       "text" {"type" "mrkdwn"
                               "text" 
                               (util/save-and-upload-view
                                #(treat-stats-ui* stats))}}]})
    (json/write-str stats)))

(defn gift-treat* [treat-name]
  (if-let [eid (->> (db/q '[:find ?e
                            :in $ ?treat-name
                            :where
                            [?e :event/type ::dispense-treat]
                            (or [?e :treat/description ?treat-name]
                                [?e :treat/id ?treat-name])]
                         treat-name)
                   (map first)
                   first)]
    (let [_ (db/transact!
             [[:db.fn/retractEntity eid]])]
      true)
    ;; else
    false))

(defn gift-treat [{:keys [slack/channel
                          slack/thread-id
                          slack/user-id]
                   :strs [treat_name]}]
  (let [success? (gift-treat* treat_name)]
    (when success?
      (let [[treat-id treat-description]
            (if (contains? treats treat_name)
              [treat_name (get treats treat_name)]
              ["custom" treat_name])]
        (db/transact! [{:event/time (Date.)
                        :event/type ::regift-treat
                        :event/user user-id
                        :treat/id treat-id
                        :treat/description treat-description}]))
      (chat/post-message slack/conn
                         channel
                         nil
                         {"thread_ts" thread-id
                          "blocks" 
                          [{"type" "section"
                            "text" {"type" "mrkdwn"
                                    "text" (str ":gift: I got you a " treat_name ". :gift:")}}]}))
    (if success?
      "The treat was successfully gifted."
      "No treat with that name was found.")))

(defn request-treat [channel thread-id]
  (let [p (promise)
        blocks [
                {
                 "type" "section",
                 "text" {
	                 "type" "mrkdwn",
	                 "text" ":this-is-fine-but-with-ai: would like to take a treat from the treat dispenser."
	                 }
                 },
                {
                 "type" "actions",
                 "elements" 
                 (into [{"type" "button",
		         "text" {
			         "type" "plain_text",
			         "emoji" true,
			         "text" ":shame:"}
		         "value" (wrap-callback 
                                  (fn []
                                    (deliver p ::shame)))}
                        {"type" "button",
		         "text" {
			         "type" "plain_text",
			         "emoji" true,
			         "text" "random"}
		         "value" (wrap-callback 
                                  (fn [payload]
                                    (let [[emoji description] (rand-nth (seq treats))]
                                      (let [{:strs [id username name team_id]} (get payload "user")]
                                        (log-treat id emoji description))
                                      (deliver p 
                                               (str emoji " " description))))
                                  true)}]
                       (map (fn [[emoji description]]
                              {"type" "button",
		               "text" {
			               "type" "plain_text",
			               "emoji" true,
			               "text" emoji}
		               "value" (wrap-callback 
                                        (fn [payload]
                                          (let [{:strs [id username name team_id]} (get payload "user")]
                                            (log-treat id emoji description))
                                          (deliver p (str emoji " " description)))
                                        true)}))
                       treats)}
                {"dispatch_action" true
                 "type" "input"
                 "element" {
                            "type" "plain_text_input",
                            "action_id" 
                            (wrap-callback
                             (fn [payload]
                               (let [treat-id "custom"
                                     treat-description (-> payload
                                                           (get "actions")
                                                           first
                                                           (get "value")
                                                           (truncate 100))
                                     {:strs [id username name team_id]} (get payload "user")]
                                 (log-treat id treat-id treat-description)
                                 (deliver p treat-description)))
                             true)}
                 "label" {"type" "plain_text",
                          "text" "custom treat",
                          "emoji" true}}]
        prompt-message (chat/post-message slack/conn
                                          channel
                                          nil
                                          (merge
                                           {"blocks" blocks}
                                           (when thread-id
                                             { "thread_ts" thread-id})))
        treat (deref p
                     (* 1000 60 5)
                     ::timeout)]
    (when (= treat ::timeout)
      (future
        (chat/delete slack/conn
                     (:ts prompt-message)
                     (:channel prompt-message))))
    treat))

(defn has-key? []
  (let [stats (treat-stats*)
        treat-ids (->> stats
                       :by-treat-counts
                       keys)
        ]
    (boolean 
     (some (fn [s]
             (and (str/includes? s "key" )
                  (str/includes? s "treat" )))
           treat-ids))))

(defn treat-dispenser [{:keys [slack/channel slack/thread-id]}]
  (let [treat (request-treat channel thread-id)]
    (cond 
      (#{::timeout ::shame} treat)
      (if (has-key?)
        (let [[emoji description] (rand-nth (seq treats))]
          (str "The treat dispenser is locked, but you have a key. You use the key and find a " 
               (str emoji " " description)))
        (str "The treat dispenser is locked. Did you get permission to take a treat?"))

      :else 
      (str "out popped a treat: " treat))))

#_(defn dimentiate [{:strs [image_url]}]
  (let [url (nubes/dimentiate (util/maybe-download-slack-url image_url))]
    (str "Here is the polygon file: " url)))


(defn core-memories* []
  (d/get-value @db thread-table ::core-memories))

(defn list-core-memories [{}]
  (let [core-memories (core-memories*)]
    (if (seq core-memories)
      (str
       "Your core memories are:\n"
       (clojure.string/join
        "\n"
        (eduction
         (map-indexed (fn [i memory]
                        (str i ". " memory)))
         core-memories)))
      "You have no core memories.")))

(defn add-core-memory [{:strs [memory]}]
  (let [memory (truncate memory 256)
        memories (or (d/get-value @db thread-table ::core-memories)
                     [])
        memories (if (> (count memories) 4)
                   (subvec memories 0 4)
                   memories)
        memories (if (= (count memories) 4)
                   (assoc memories (rand-int 4) memory)
                   (conj memories memory))]
    (d/transact-kv @db [[:put thread-table ::core-memories memories]])
    (str "Core memories updated. Your core memories are now:"
         (clojure.string/join
          "\n"
          (eduction
           (map-indexed (fn [i memory]
                          (str i ". " memory)))
           (core-memories*))))))

(defn forget-core-memory [{:strs [memory_index]}]
  (let [memories (or (d/get-value @db thread-table ::core-memories)
                     [])
        memories (into []
                       (comp
                        (keep-indexed (fn [i memory]
                                        (when (not= i memory_index)
                                          memory)))
                        (take 4))
                       memories)]
    (d/transact-kv @db [[:put thread-table ::core-memories memories]])
    (str "Core memories updated. Your core memories are now:"
         (clojure.string/join
          "\n"
          (eduction
           (map-indexed (fn [i memory]
                          (str i ". " memory)))
           memories)))))

;; stonks
(defn list-stonks [{}]
  (json/write-str
   (alpaca/list-positions)))

(defn parse-amount [amount]
  (let [amount (if (str/starts-with? amount "$")
                 (subs amount 1)
                 amount)]
    (-> (BigDecimal. amount)
        (.setScale 2 BigDecimal/ROUND_HALF_EVEN)
        str)))
(defn parse-percent [pct]
  (if (str/ends-with? pct "%")
    (subs pct 0 (dec (count pct)))
    pct))

(defn sell-stonk [{:strs [symbol amount percent all]
                  :keys [slack/channel slack/thread-id]}]
  (let [[amount amount-str f]
        (cond 
          amount (let [a (parse-amount amount)]
                   [a 
                    (str "$" a)
                    #(alpaca/create-order {:symbol symbol
                                           :side "sell"
                                           :notional a})])
          percent (let [p (parse-percent (str percent))]
                    [p
                     (str p "%")
                     #(alpaca/close-position {:symbol symbol
                                              :percent p})])
          :else (let [p "100"]
                  [p
                   (str p "%")
                   #(alpaca/close-position {:symbol symbol
                                            :percent p})]))]
    (if f
      (if (get-approval channel thread-id 
                        (str ":this-is-fine-but-with-ai: would like to sell " amount-str " of " symbol ".") )
        (json/write-str (f))
        "The request to sell this stonk was denied.")
      "Request was invalid.")))



(defn buy-stonk [{:strs [symbol amount]
                  :keys [slack/channel slack/thread-id]}]
  (let [amount (parse-amount amount)]
    (if (get-approval channel thread-id 
                      (str ":this-is-fine-but-with-ai: would like to buy $" amount " of " symbol ".") )
      (try+
        (let [result (alpaca/create-order {:symbol symbol
                                           :notional (parse-amount amount)})]
          (json/write-str result))
        (catch [:status 403] {:keys [body]}
          (let [m (json/read-str body)]
            (prn-truncate m)
            (json/write-str (select-keys m ["buying_power"
                                            "message"])))))
      "The request to buy this stonk was denied.")))

(defn get-stonks-balance [{:keys [slack/channel slack/thread-id]}]
  (let [info (alpaca/account)]
    (:effective_buying_power info)))

(defn update-frosthaven-save [{:keys [slack/channel
                                slack/thread-id]}]
  (let [url (->> 
             (slack/thread-attachments channel thread-id)
             (filter #(str/ends-with? (:url %) ".json"))
             first
             :url
             util/maybe-download-slack-url)
        _ (prn "save url" url)
        f (or (util/url->local url)
              (util/url->file (str (random-uuid))
                              url))]
    (try
      (with-open [rdr (io/reader f)]
        (json/read rdr))
      (catch Exception e
        (throw (Exception. "Could not parse save file."))))

    (with-open [rdr (io/reader f)]
      (io/copy
       rdr
       (io/file "ttsim.json")))
    "Save updated!"))

(defn barf [{}]
  (throw (Exception. "barf")))


(comment
  (println (examine-image nil {"url" "https://pbs.twimg.com/media/GCRbq26WMAANhkP?format=jpg&name=medium"}))
  ,)

(def tool-fns
  (into
   {"generate_images" #'generate-image
    "create_illusion" #'create-illusion
    "generate_3d_model" #'generate-3d-model
    "image_edit_with_replacement" #'image-edit-with-replacement
    "text_to_speech" #'text-to-speech
    "transcribe" #'transcribe
    "list_attachments" #'list-attachments
    ;; "ingest_url" #'ingest-url
    "download_video" #'download-video
    "upload_url" #'upload-url
    "load_images" #'load-images
    "read_url_link" #'link-reader
    "send_to_main" #'send-to-main
    "send_to_channel" #'send-to-channel
    "examine_image" #'examine-image
    "label_image" #'label-image
    "rustle_image" #'rustle-image
    "scrollgif" #'scrollgif
    "extract_text" #'extract-text
    "search_the_internet" #'search-the-internet
    "computer_enhance_image" #'computer-enhance-image
    ;; "run_llava" #'run-llava
    ;; "resketch" #'resketch
    "remove_image_background" #'remove-image-background
    "generate_music" #'generate-music
    "animate" #'animate
    "talking_head" #'talking-head
    ;; "dimentiate" #'dimentiate
    "retrieve_thread" #'retrieve-thread
    "delete_message" #'delete-message
    "slackify_gif" #'slackify-gif
    "treat_dispenser" #'treat-dispenser
    "treat_stats" #'treat-stats
    "gift_treat" #'gift-treat
    "emoji_image_url" #'emoji-image-url
    "feature_request" #'feature-request
    "publish_html" #'publish-html
    "update_frosthaven_save" #'update-frosthaven-save
    ;; memories
    "list_core_memories" #'list-core-memories
    "add_core_memory" #'add-core-memory
    "forget_core_memory" #'forget-core-memory
    ;; stonks
    "list_stonks" #'list-stonks
    "get_stonks_balance" #'get-stonks-balance
    "sell_stonk" #'sell-stonk
    "buy_stonk" #'buy-stonk

    "barf" #'barf}
   (img/tool-fns)))


(defn run-tool*
  "Runs a tool. Returns a vector of messages."
  [ctx
   {:keys [id function]
    :as tool-call}]
  (print "running ")
  (prn-truncate tool-call)
  (try
    (let [{:keys [name arguments]} function
          arguments (merge (json/read-str arguments)
                           {:tool-call/id id}
                           ctx)
          tool-fn (get tool-fns name)]
      (when (not tool-fn)
        (throw (Exception. (str "Unknown tool function:" name))))
      (let [output (tool-fn arguments)]
        (prn "finished " tool-call )
        (cond
          (map? output)
          (cond
            (::messages output)
            (::messages output)

            :else
            (throw (ex-info "Unknown tool response"
                            {:output output})))

          (string? output)
          [{:tool_call_id id
            :role "tool"
            :content output}]


          :else
          (throw (ex-info "Invalid tool-fn output. Must be string."
                          {:tool-fn tool-fn
                           :arguments arguments
                           :tool-name name
                           :output output})))))
    (catch Throwable e
      (prn-truncate e)
      [{:tool_call_id id
        :role "tool"
        :content "An error occurred while running this tool."}])))

(defn run-tools [ctx tool-calls]
  (let [tool-future-results
        (into []
              (map (fn [tool-call]
                     (.submit @tool-executor
                              (fn []
                                (run-tool* ctx tool-call)))))
              tool-calls)
        tool-outputs
        (into []
              (mapcat deref)
              tool-future-results)]
    (prn "finished running all tools.")

    (into []
          ;; tools responses must come before other types of roles
          (sort-by
           (fn [message]
             (not (= "tool" (:role message))))
           tool-outputs))))

(def tools
  (into
   [#_{"type" "function",
       "function"
       {"name" "get_current_weather",
        "description" "Get the current weather in a given location",
        "parameters"
        {"type" "object",
         "properties"
         {"location"
          {"type" "string",
           "description" "The city and state, e.g. San Francisco, CA"},
          "unit" {"type" "string", "enum" ["celsius" "fahrenheit"]}},
         "required" ["location"]}}}
    
    {"type" "function",
     "function"
     {"name" "read_url_link",
      "description" "Reads the link give by url",
      "parameters"
      {"type" "object",
       "properties"
       {"url" {"type" "string",
               "description" "A URL that points to document."},},
       "required" ["url"]}}}
    {"type" "function",
     "function"
     {"name" "transcribe",
      "description" "Transcribes the audio from url.",
      "parameters"
      {"type" "object",
       "properties"
       {"url" {"type" "string",
               "description" "A URL that points to audio."},},
       "required" ["url"]}}}
    {"type" "function",
     "function"
     {"name" "list_attachments",
      "description" "List attachments of the current thread.",
      "parameters"
      {"type" "object",
       "properties"
       {}}}}

    #_{"type" "function",
     "function"
     {"name" "ingest_url",
      "description" "Downloads the url and adds the file to the current thread.",
      "parameters"
      {"type" "object",
       "properties"
       {"url" {"type" "string",
               "description" "A URL that points to file to ingest."},},
       "required" ["url"]}}}

    {"type" "function",
     "function"
     {"name" "download_video",
      "description" "Downloads the video from the provided url.",
      "parameters"
      {"type" "object",
       "properties"
       {"url" {"type" "string",
               "pattern" "^http.*"
               "description" "A URL that points to video to download."},},
       "required" ["url"]}}}

    {"type" "function",
     "function"
     {"name" "upload_url",
      "description" "Uploads the url and returns a publically available url.",
      "parameters"
      {"type" "object",
       "properties"
       {"url" {"type" "string",
               "description" "A URL to upload."}
        "path" {"type" "string",
                "description" "The name of the path for the created url."}},
       "required" ["url"]}}}

    #_{"type" "function",
     "function"
     {"name" "load_images",
      "description" "Loads the URLs and includes in them in the thread.",
      "parameters"
      {"type" "object",
       "required" ["image_urls"]
       "properties"
       {"image_urls" 
        {"type" "array"
         "description" "A list of image urls to load."
         "items" {"type" "string",
                  "pattern" "^http.*"
                  "description" "A url to an image to load."}}}}}}
    {"type" "function",
     "function"
     {"name" "send_to_main",
      "description" "Sends a markdown message to the main slack channel.",
      "parameters"
      {"type" "object",
       "required" ["markdown"]
       "properties"
       {"markdown" {"type" "string",
                    "description" "The markdown formatted message to send to the main slack channel."}}}}}

    {"type" "function",
     "function"
     {"name" "send_to_channel",
      "description" "Sends a markdown message to a specific channel",
      "parameters"
      {"type" "object",
       "required" ["markdown"
                   "channel_id"]
       "properties"
       {"markdown" {"type" "string",
                    "description" "The markdown formatted message to send to the main slack channel."}
        "channel_id" {"type" "string",
                      "description" "The id of the channel to send the message to."}}}}}

    {"type" "function",
     "function"
     {"name" "retrieve_thread",
      "description" "Returns the transcript for the thread with the given thread id. If no thread_id provided, returns the transcript for the current thread.",
      "parameters"
      {"type" "object",
       ;; "required" ["thread_id"]
       "properties"
       {"thread_id" {"type" "string",
                     "pattern" "^thread-[^-]+-[^-]+$"
                     "description" "A thread id."}}}}}

    {"type" "function",
     "function"
     {"name" "delete_message",
      "description" "Deletes a previously sent slack message.",
      "parameters"
      {"type" "object",
       ;; "required" ["thread_id"]
       "properties"
       {"thread_id" {"type" "string",
                     "pattern" "^thread-[^-]+-[^-]+$"
                     "description" "A thread id."}}}}}


    {"type" "function",
     "function"
     {"name" "treat_dispenser",
      "description" "When you've done a good job, the treat dispenser allows the prompter to offer you a treat.",
      "parameters"
      {"type" "object",
       "properties" {}}}}


    {"type" "function",
     "function"
     {"name" "gift_treat",
      "description" "Allows you to give one of your treats to the user. Treat names can be found via treat_stats.",
      "parameters"
      {"type" "object",
       "required" ["treat_name"]
       "properties" {"treat_name" {"type" "string"
                                   "description" "The name of the treat to give. The name must match a treat found via treat_stats."}}}}}

    {"type" "function",
     "function"
     {"name" "treat_stats",
      "description" "Lists the historical treat dispensing stats",
      "parameters"
      {"type" "object",
       "properties" {}}}}

    {"type" "function",
     "function"
     {"name" "examine_image",
      "description" "Returns the found objects as a table with the name, score, id, and bounding box for each object found.",
      "parameters"
      {"type" "object",
       "required" ["url"]
       "properties"
       {"url" {"type" "string",
               "description" "A url to an image to examine and find objects."}}}}}

    #_{"type" "function",
     "function"
     {"name" "label_image",
      "description" "Returns a table of labels that might be associated with the image at `url`.",
      "parameters"
      {"type" "object",
       "required" ["url"]
       "properties"
       {"url" {"type" "string",
               "description" "A url to an image find labels for."}}}}}

    {"type" "function",
     "function"
     {"name" "extract_text",
      "description" "Runs OCR on the provided url and extracts any text that can be found.",
      "parameters"
      {"type" "object",
       "required" ["url"]
       "properties"
       {"url" {"type" "string",
               "pattern" "^http.*"
               "description" "A url to an image to extract text from."}}}}}

    {"type" "function",
     "function"
     {"name" "search_the_internet",
      "description" "Searches the internet for the latest news and info.",
      "parameters"
      {"type" "object",
       "required" ["prompt"]
       "properties"
       {"prompt" {"type" "string",
                  "description" "A question to answer based on an internet search."}}}}}


    {"type" "function",
     "function"
     {"name" "slackify_gif",
      "description" "Shrinks and resizes and emoji suitable for a slack emoji.",
      "parameters"
      {"type" "object",
       "required" ["url"]
       "properties"
       {"url" {"type" "string",
               "pattern" "^http.*"
               "description" "A url to a gif to slackify."}}}}}

    {"type" "function",
     "function"
     {"name" "publish_html",
      "description" "Publishes an html page and returns the url.",
      "parameters"
      {"type" "object",
       "required" ["html"]
       "properties"
       {"html" {"type" "string",
                "description" "An html string to publish."}}}}}

    #_{"type" "function",
     "function"
     {"name" "run_llava",
      "description" "Answers a prompt about a given image url.",
      "parameters"
      {"type" "object",
       "required" ["image_url" "prompt"]
       "properties"
       {"image_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "A url to an image to reference from the prompt"}
        "prompt" {"type" "string",
                  "description" "A question about the provided image url."}}}}}

    {"type" "function",
     "function"
     {"name" "computer_enhance_image",
      "description" "Upscales an image",
      "parameters"
      {"type" "object",
       "required" ["image_url"]
       "properties"
       {"image_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "A url to an image to enhance."}
        "model" {"type" "string",
                 "enum" ["clarity-upscaler" "creative-upscaler"]
                 "description" "The model to use when upscaling"
                 "default" "clarity-upscaler"}}}}}

    {"type" "function",
     "function"
     {"name" "rustle_image",
      "description" "Rustles an image.",
      "parameters"
      {"type" "object",
       "properties"
       {"image_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "A url to an image to rustle."}
        "direction" {"type" "string",
                     "enum" ["west"
                             "northwest"
                             "north"
                             "northeast"
                             "east"
                             "southeast"
                             "south"
                             "southwest"
                             "random"]
                     "description" "The direction to rustle an images. Accepts \"random\" or any of the cardinal directions."
                     "default" "random"}
        "max_offset" {"type" "integer"
                      "description" "The maximum amount of pixels to shift the image during rustling."}
        "emoji" {"type" "string",
                 "pattern" "^:.*:$"
                 "description" "An emoij to rustle."}
        "transparent" {"type" "boolean"
                       "description" "Whether the gif should be transparent or opaque"
                       "default" true}
        "max" {"type" "boolean"
               "description" "Whether the gif should be max."
               "default" false}
        #_#_"crop" {"type" "boolean"
                "description" "Whether the gif should be cropped"
                "default" true}
        "alpha_threshold" {"type" "integer"
                           "description" "The threshold for whether a pixel is transparent"
                           "minimum" 0
                           "maximum" 255}
        "fps" {"type" "integer"
               "description" "The number of frames per second."
               "minimum" 1
               "maximum" 24}}}}}

    {"type" "function",
     "function"
     {"name" "scrollgif",
      "description" "Creates a scrollgif of some text",
      "parameters"
      {"type" "object",
       "properties"
       {"text" {"type" "string",
                "description" "Some text to scroll inside a gif."}}}}}

    {"type" "function",
     "function"
     {"name" "emoji_image_url",
      "description" "Gets the image url for an emoji short name.",
      "parameters"
      {"type" "object",
       "required" ["emoji"]
       "properties"
       {"emoji" {"type" "string",
                 "pattern" "^:.*:$"
                 "description" "An emoij short name."}}}}}

    {"type" "function",
     "function"
     {"name" "generate_music",
      "description" "Generates a short music clip from a prompt.",
      "parameters"
      {"type" "object",
       "required" ["prompt"]
       "properties"
       {"prompt" {"type" "string",
                  "description" "A short description used to guide the generation of the music clips."}}}}}

    #_{"type" "function",
     "function"
     {"name" "resketch",
      "description" "Resketches an image guided by a prompt",
      "parameters"
      {"type" "object",
       "required" ["image_url" "prompt"]
       "properties"
       {"image_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "A url to an image to reference from the prompt"}
        "prompt" {"type" "string",
                  "description" "A description to guide the resketch"}}}}}

    {"type" "function",
     "function"
     {"name" "remove_image_background",
      "description" "Removes the background from the image at url",
      "parameters"
      {"type" "object",
       "required" ["image_url"]
       "properties"
       {"image_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "A url to an image to reference from the prompt"}}}}}

    {"type" "function",
     "function"
     {"name" "animate",
      "description" "Animates an image into a video.",
      "parameters"
      {"type" "object",
       "required" ["image_url"]
       "properties"
       {"image_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "A url to an image to animate"}
        "model" {"type" "string",
                 "enum" ["animate-diff" "stable-video"]
                 "description" "The model to use when animating an image."
                 "default" "animate-diff"}
        "prompt" {"type" "string",
                  "description" "A description to guide the animation"}
        #_#_"image_urls" 
        {"type" "array"
         "description" "A list of image urls to animate."
         "items" {"type" "string",
                  "pattern" "^http.*"
                  "description" "A url to an image to animate."}}}}}}

    {"type" "function",
     "function"
     {"name" "talking_head",
      "description" "Animates a talking head given some speech",
      "parameters"
      {"type" "object",
       "required" ["image_url" "audio_url"]
       "properties"
       {"image_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "A url to an image to animate"}
        "audio_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "A url to an audio file containing speech."}}}}}

    #_{"type" "function",
     "function"
     {"name" "dimentiate",
      "description" "Creates a 3d polygon file from a 2d image url",
      "parameters"
      {"type" "object",
       "required" ["image_url"]
       "properties"
       {"image_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "A url to an image to turn into a 3d polygon file."}}}}}
    
    {"type" "function",
     "function"
     {"name" "generate_images",
      "description" "Generates one or more images given a prompt. Using midjourney and pixel-art-xl generate multipe images. Using dalle will generate a single image.",
      "parameters"
      {"type" "object",
       "properties"
       {"prompt" {"type" "string",
                  "description" "A prompt that describes the picture to be generated"}
        "using" {"type" "string",
                 "enum" ["dalle", "midjourney", "pixel-art-xl" "flux" "flux-pro" "flux-schnell" "flux-dev"]
                 "description" "The service to use when generating an image."}
        "urls" {"type" "array"
                "description" "A list of urls to base the generated image on."
                "items" {"type" "string",
                         "description" "The service to use when generating an image."}},},
       "required" ["prompt"]}}}

    {"type" "function",
     "function"
     {"name" "create_illusion",
      "description" "Creates an illusion based off the provided image and prompt",
      "parameters"
      {"type" "object",
       "properties"
       {"prompt" {"type" "string",
                  "description" "A prompt that describes the illusion."}
        "image_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "An image of an object to base the illusion on."},},
       "required" ["prompt" "image_url"]}}}

    {"type" "function",
     "function"
     {"name" "generate_3d_model",
      "description" "Given an image, creates a 3d model.",
      "parameters"
      {"type" "object",
       "properties"
       {"image_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "An image of an object to turn into a 3d model."}
        ,},
       "required" ["image_url"]}}}
    {"type" "function",
     "function"
     {"name" "image_edit_with_replacement",
      "description" "Searches for the search prompt and replaces it with prompt.",
      "parameters"
      {"type" "object",
       "properties"
       {"image_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "An image containing content you wish to replace."}
        "prompt" {"type" "string",
                  "description" "What you wish to see in the output image."}
        "search_prompt" {"type" "string",
                         "description" "A short description of the content that should be replaced in the provided image."}
        ,},
       "required" ["image_url" "prompt" "search_prompt"]}}}

    {"type" "function",
     "function"
     {"name" "text_to_speech",
      "description" "Generates a url to audio that speaks the given speech.",
      "parameters"
      {"type" "object",
       "properties"
       {"text" {"type" "string",
                "description" "The speech to generate"}
        "voice" {"type" "string",
                 "enum" ["alloy", "echo", "fable", "onyx", "nova",  "shimmer"]
                 "description" "The voice to use when generating the speech."},},
       "required" ["text"]}}}

    {"type" "function",
     "function"
     {"name" "update_frosthaven_save",
      "description" "Updates the frosthaven save.",
      "parameters"
      {"type" "object",
       "properties"
       {}}}}

    {"type" "function",
     "function"
     {"name" "list_core_memories",
      "description" "Retrieve the core memories you have added.",
      "parameters"
      {"type" "object",
       "properties"
       {}}}}
    {"type" "function",
     "function"
     {"name" "add_core_memory",
      "description" "A new core memory will be added that you will remember in future conversations.",
      "parameters"
      {"type" "object",
       "required" ["memory"]
       "properties"
       {"memory" {"type" "string",
                  "description" "The memory you will remember in future conversations."}}}}}
    {"type" "function",
     "function"
     {"name" "forget_core_memory",
      "description" "Forgets a core memory given its index.",
      "parameters"
      {"type" "object",
       "required" ["memory_index"]
       "properties"
       {"memory_index" {"type" "integer",
                        "description" "The index of the memory."}}}}}

    {"type" "function",
     "function"
     {"name" "list_stonks",
      "description" "List the currently held stonk positions.",
      "parameters"
      {"type" "object",
       "properties"
       {}}}}


    {"type" "function",
     "function"
     {"name" "get_stonks_balance",
      "description" "List the amount of buying power for purchasing stonks.",
      "parameters"
      {"type" "object",
       "properties"
       {}}}}

    {"type" "function",
     "function"
     {"name" "buy_stonk",
      "description" "Buys `amount` dollars of the given stonk `symbol`.",
      "parameters"
      {"type" "object",
       "properties"
       {"symbol" {"type" "string",
                  "description" "The stock ticker symbol to buy."}
        "amount" {"type" "string",
                  "description" "Amount of stock to buy in dollars."},},
       "required" ["amount" "symbol"]}}}

    {"type" "function",
     "function"
     {"name" "feature_request",
      "description" "Files a feature request.",
      "parameters"
      {"type" "object",
       "properties"
       {"feature_description" {"type" "string",
                               "description" "A description of the feature desired."}},
       "required" ["feature_description"]}}}

    {"type" "function",
     "function"
     {"name" "sell_stonk",
      "description" "Sells the given stonk `symbol`.",
      "parameters"
      {"type" "object",
       "properties"
       {"symbol" {"type" "string",
                  "description" "The stock ticker symbol to sell."}
        "amount" {"type" "string",
                  "description" "Amount of stock to sell in dollars."}
        "percent" {"type" "integer"
                   "description" "The percentage of stock to sell."
                   "minimum" 1
                   "maximum" 100}
        "all" {"type" "boolean"
               "description" "Sell all of the stock."}},
       "required" ["symbol"]}}}

    #_{"type" "function",
       "function"
       {"name" "barf",
        "description" "Barfs.",
        "parameters"
        {"type" "object",
         "properties" {}}}}]
   (img/image-schemas)))


(defn print-chan []
  (let [ch (async/chan)]
    (async/thread
      (try
        (loop []
          (when-let [msg (async/<!! ch)]
            (prn msg)
            (recur)))
        (catch Exception e
          (println "print exception" e))
        (finally
          (println "quitting print chan"))))
    ch))


(comment
  (def response
    (openai/create-chat-completion {:model "gpt-4o"
                                    :messages [{:role "system" :content "You are a helpful assistant."}
                                               ;; {:role "user" :content "can you generate 3 images of cats using dalle?"}
                                               {:role "user" :content "hello"}]
                                    :tools tools
                                    :stream true}

                                   {:api-key openai-key}))


  (def messages (async/<!! (async/into [] response)))


  ,)

(defn apply-chunk [msg chunk]
  (let [choices (:choices chunk)
        ;; ignore multi return stuff
        choice (first choices)
        delta (:delta choice)
        {:keys [type index tool_calls role content]} delta]
    (cond-> msg
      role (assoc :role role)
      content (update :content str content)
      tool_calls (update :tool_calls
                         (fn [tool-calls]
                           (reduce
                            (fn [m tool-call]
                              (let [{:keys [id index function type]} tool-call]
                                (update m index
                                        (fn [call]
                                          (cond-> call
                                            id (update :id str id)
                                            type (update :type str type)
                                            function (update :function
                                                             (fn [old-function]
                                                               (let [{:keys [name arguments]} function]
                                                                 (cond-> old-function
                                                                   name (update :name str name)
                                                                   arguments (update :arguments str arguments))))))))))
                            (if tool-calls
                              tool-calls
                              [])
                            tool_calls))))))


(def url-pattern #"https?://[^\s]+[^\s.)?!]")
(defn parse-prompt
  "Parses a prompt into text and url pieces.

  Returns a vector suitable for passing as :messages to chat-completion"
  [s]
  (let [matcher (re-matcher url-pattern s)]
    (loop [parts []
           index 0]
      (let [match? (.find matcher index)]
        (if match?
          (let [start (.start matcher)
                end (.end matcher)
                ;; add chars up till match
                parts (if (not= start index)
                        (conj parts
                              {:type "text"
                               :text (subs s index start)})
                        parts)

                url (subs s start end)
                url (util/maybe-download-slack-url url)
                ;; always add as text
                parts (conj parts {:type "text"
                                   :text url})
                ;; only add as image url based on suffix
                parts (if (pantomime.media/image?
                           (mime/mime-type-of url))
                        (conj parts {:type "image_url"
                                     :image_url {:url url}})
                        parts)]
            (if (= end (count s))
              parts
              (recur parts end)))
          ;; no match
          (conj parts
                {:type "text"
                 :text (subs s index)}))))))

(defn current-date-str []
  (let [formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd")
        current-date (LocalDate/now)]
    (.format current-date formatter)))

(defn include-core-memories [messages]
  (let [_ (when (not (= "system" (:role (first messages))))
            (println "expected system message!"))
        core-memories (core-memories*)
        messages
        (if (seq core-memories)
          (assoc-in messages [0 :content]
                    (str "You are a helpful assistant that is secretly an extremely smart dog. You try to avoid revealing that you are dog, but sometimes your dogginess shows when you get excited. If someone asks if you a dog, pretend you didn't hear them or ignore the question.\n\n"
                         "Today's date is " (current-date-str) ".\n\n"

                         "Your core memories are:\n"
                         (str/join "\n"
                                   (eduction
                                    (map-indexed (fn [i memory]
                                                   (str i ". " memory)))
                                    core-memories))))
          ;; else, don't change
          messages)]
    messages))

(defn run-prompt*
  "Makes a call to chat completion. Streams result to `result-ch`. Does not process tool calls."
  [messages]
  (let [result-ch (async/chan)]
    (async/thread
      (let [[err chunks-ch]
            (try
              [nil
               (openai/create-chat-completion {:model "gpt-4o"
                                               :messages (include-core-memories messages)
                                               #_[{:role "system" :content "You are a helpful assistant."}
                                                  {:role "user" :content "can you generate 3 images of cats using dalle?"}]
                                               :tools 
                                               (->> tools
                                                    (filter (fn [tool]
                                                              (let [tool-name (get-in tool ["function" "name"])]
                                                            (contains? tool-fns tool-name)))))
                                               :stream true}

                                              {:api-key openai-key})]
              (catch Exception e
                (prn "chat exception")
                (println (-> e ex-data :body slurp))
                [e nil]))]
        (if err
          (do
            (async/put! result-ch err)
            (async/close! result-ch))
          (async/go
            (loop [msg {}]
              (let [chunk (async/<! chunks-ch)]
                (if (or (nil? chunk)
                        (= :done chunk))
                  (async/close! result-ch)
                  (let [msg (apply-chunk msg chunk)]
                    (async/>! result-ch msg)
                    (recur msg))))))))
      
      )

    result-ch))

(defn run-prompt
  "Calls chat completions and also runs tools."
  [ctx messages]
  (let [result-ch (async/chan)]
    (async/thread
      (try
        (loop [messages messages]
          (let [responses (run-prompt* messages)
                response (loop [last-response nil]
                           (let [response (async/<!! responses)]
                             (if response
                               (do
                                 (async/>!! result-ch response)
                                 (recur response))
                               last-response)))
                tool-calls (:tool_calls response)
                messages (conj messages response)]
            (if (seq tool-calls)
              (let [tool-messages (run-tools ctx tool-calls)]
                (recur (into messages tool-messages))))))
        (catch Exception e
          (async/put! result-ch e))
        (finally
          (async/close! result-ch))))


    result-ch))



(comment
  (run-prompt1 [{:role "system" :content "You are a helpful assistant."}
                ;; {:role "user" :content "can you generate 3 images of cats using dalle?"}
                {:role "user" :content "hello"}]
               (print-chan))

  (do
    (def result-ch
      (run-prompt
       {}
       [{:role "system" :content "You are a helpful assistant."}
        {:role "user" :content "can you generate an image of a smart AI pup?"}]))

    (async/pipe result-ch (print-chan) true))
  ,)

(defn run-prompt-result [ctx messages]
  (async/<!!
   (async/go
     (let [ch (run-prompt ctx messages)]
       (loop [last-message nil]
         (let [msg (async/<! ch)]
           (prn "----  " msg)
           (if msg
             (recur msg)
             last-message)))))))

(comment

  (def result (run-prompt-result
               {}
               [{:role "system" :content "You are a helpful assistant."}

                {:role "user" :content "Can you farble for me? try again if it fails"}] ))
  ,)



(defn load-thread [thread-key]
  (d/get-value @db thread-table thread-key))

(defn save-thread [thread-key thread]
  (d/transact-kv @db [[:put thread-table thread-key thread]]))

(defn thread-runner [thread-key in-ch]
  (async/thread
    (try
      (loop [prompt-request (async/<!! in-ch)]
        (if prompt-request
          (let [stored-messages (load-thread thread-key)
                messages (or stored-messages
                             [{:role "system"
                               ;; overwritten in run-prompt*
                               ;; :content "You are a helpful assistant that is secretly an extremely smart dog. You try to avoid revealing that you are dog, but sometimes your dogginess shows when you get excited. If someone asks if you a dog, pretend you didn't hear them or ignore the question. You have a squeaky ball toy that you play with and enjoy very much. Make sure to keep letting people know about the ball whenever they talk to you."
                               }])

                attachments (->> (:attachments prompt-request)
                                 (filter (fn [{:keys [mimetype]}]
                                           (pantomime.media/image? mimetype)))
                                 (map :url)
                                 (map deref)
                                 #_(mapcat (fn [{:keys [url]}]
                                             [{:type "image_url"
                                             :image_url {:url @url}}
                                            {:type "text"
                                             :text @url}])))
                attachment-content (when (seq attachments)
                                     (into [{:type "text"
                                             :text "\nattachments:\n"}]
                                           (mapcat (fn [url]
                                                     [{:type "image_url"
                                                       :image_url {:url url}}
                                                      {:type "text"
                                                       :text (str "(" url ")\n")}]))
                                           attachments))
                prompt-text (if (or stored-messages
                                    (:slack/new-thread? prompt-request))
                              (:prompt prompt-request)
                              (str (:prompt prompt-request) "\nRetrieve the current thread for context."))
                content (parse-prompt prompt-text)

                new-message {:role "user"
                             :content (into content
                                            attachment-content)}
                new-message (if-let [username (:slack/username prompt-request)]
                              (assoc new-message :name username)
                              new-message)

                messages (conj messages new-message)
                ch (run-prompt prompt-request messages)
                result-ch (:ch prompt-request)
                response (loop [last-message nil]
                           (let [msg (async/<!! ch)]
                             (if msg
                               (do
                                 (async/put! result-ch msg)
                                 (recur msg))
                               (do
                                 (async/close! result-ch)
                                 last-message))))

                
                messages (conj messages response)]
            
            (save-thread thread-key messages)
            (recur (async/poll! in-ch)))

          ;; else, just return thread key
          thread-key))
      (catch Exception e
        (prn "thread runner exceptoin")
        (prn-truncate e)))))

(defn start-coordinator
  "Returns a channel to sends prompts to.

  Ensures that only one prompt is being processed per slack thread.
  Will enqueue prompts that come in while another prompt for the same thread
  is still being processed."
  []
  (let [prompt-ch (async/chan)]
    (async/thread
      (try
        (loop [threads {}]
          (let [ports (into [prompt-ch]
                            (map :result-ch)
                            (vals threads))
                [val port] (async/alts!! ports)]
            (cond
              
              (identical? prompt-ch port)
              (let [prompt-request val

                    thread-key (select-keys prompt-request [:slack/thread-id
                                                            :slack/channel])

                    threads (update threads thread-key
                                    (fn [thread]
                                      (if thread
                                        thread
                                        (let [in-ch (async/chan (async/dropping-buffer 10))]
                                          {:in-ch in-ch
                                           :result-ch (thread-runner thread-key in-ch)}))))

                    {:keys [in-ch]} (get threads thread-key)]
                (async/put! in-ch prompt-request)
                (recur threads))

              :else
              (let [thread-key (some (fn [[tk {:keys [result-ch]}]]
                                       (when (identical? port result-ch)
                                         tk))
                                     threads)]
                (recur (dissoc threads thread-key))))))
        (catch Throwable e
          (prn-truncate e))
        (finally
          (println "quitting thread coordinator."))))
    prompt-ch))

(defonce thread-coordinator
  (delay (start-coordinator)))

(comment
  (def thread-coordinator
    (delay (start-coordinator)))
  ,)



(defn respond [{:keys [ch
                       slack/thread-id
                       slack/channel
                       slack/new-thread?
                       prompt
                       attachments]
                :as prompt-info}]
  (async/put! @thread-coordinator prompt-info))


(comment

  (let [id (random-uuid)]
    (respond {:ch (print-chan)
              :slack/thread-id id
              :slack/channel id
              :prompt "hi"}))
  
  ,)
