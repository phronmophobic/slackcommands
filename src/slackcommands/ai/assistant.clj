(ns slackcommands.ai.assistant
  (:require [wkok.openai-clojure.api :as openai]
            [clojure.java.shell :as sh]
            [clojure.edn :as edn]
            [com.phronemophobic.discord.api :as discord]
            [com.phronemophobic.clj-media :as clj-media]
            [clojure.core.async :as async]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [com.phronemophobic.discord.api :as discord]
            [com.phronemophobic.nubes :as nubes]
            [slackcommands.util :as util]
            [slackcommands.gif :as gif]
            [slackcommands.ai.img :as img]
            [slackcommands.slack :as slack]
            [slackcommands.ai.vision :as vision]
            [membrane.ui :as ui]
            [clojure.java.io :as io]
            [clj-http.client :as client]
            [clojure.string :as str])
  (:import java.util.concurrent.Executors))

(def openai-key
  (:chatgpt/api-key
   (edn/read-string (slurp "secrets.edn"))))

(def assistant-id "asst_FFFmAt4eemrv3BZ8TFUq4Ob5")
;; {thread-id {id {:id id
;;                 :url @url
;;                 :mimetype mimetype}}}
(defonce thread-attachments
  (atom {}))

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

(defn text-to-speech [_thread-id
                      {:strs [text voice]}]
  (let [voice (or voice "alloy")
        is (openai/create-speech {:model "tts-1"
                                  :input text
                                  :voice voice
                                  :response_format "mp3"}
                                 {:api-key openai-key})
        fname (str (sanitize-name (truncate text 15)) ".mp3")
        url (util/save-and-upload-stream fname is)]
    url))


;; not all audio formats are transcrible.
;; eg. iphone created audio.
(defn fix-audio [f]
  (let [output (io/file "/var/tmp/transcribe.mp3")]
    (clj-media/write!
     (->> (clj-media/file (.getCanonicalPath f))
          (clj-media/filter-audio))
     (.getCanonicalPath output))
    output))

(defn transcribe [_thread-id
                  {:strs [url]}]
  (let [f (or (util/url->local url)
              (util/url->file (str (random-uuid))
                              url))
        _ (prn "transcribing" f (.exists f))
        response (openai/create-transcription
            {:model "whisper-1"
             :file (fix-audio f)}
            {:api-key openai-key})]
    (prn "transcription" response)
    (json/write-str response)
    #_(:text response)))

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

(defn link-reader [_thread-id
                   {:strs [url]}]
  (let [response (http/get url
                           {:as :stream})
        content-type (get-in response [:headers "Content-Type"])]
    (case content-type
      (;; "image/jpeg" "image/png"
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




(defn generate-image [_thread-id
                      {:strs [prompt
                              using
                              urls]}]
  (let [prompt (if (and (seq urls)
                        (not= "dalle" using))
                 (str (str/join " " urls) " "
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


(defn list-attachments [thread-id {:strs [type]}]
  (let [pred (if type
               (case type
                 "audio" util/audio?
                 "video" util/video?
                 "image" util/image?
                 "plaintext" util/plaintext?)
               (constantly true))
        urls
        (->> (get @thread-attachments thread-id)
             vals
             (filter #(pred (:mimetype %)))
             (map :url)
             (map deref))]
    (if (seq urls)
      (str/join "\n" urls)
      "No matching attachments.")))

(defn send-to-main [thread-id {:strs [markdown]}]
  ;; (slack/send-to-main markdown)
  "Your send to main privileges have been revoked!")


(defn retrieve-thread [_thread-id {:strs [thread_id]}]
  (let [[_ channel-id thread-id] (str/split thread_id #"-")]
    (prn channel-id thread-id)
    (if-let [s (slack/retrieve-thread channel-id thread-id)]
      (str "Below is a transcript of the thread:\n\n" s)
      "No thread found for that thread id.")))

(defn examine-image [_thread-id {:strs [url]}]
  (let [objs (vision/find-objects url)]
    (if (seq objs)
      (let [img-url (util/save-and-upload-view
                     #(vision/highlight-objects url objs))
            [w h] (ui/bounds (ui/image (io/as-url url)))]
        (str
         "Image URL: " img-url "\n\n"
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
             objs)))))
      "No objects found.")))

(defn computer-enhance-image [_thread-id {:strs [image_url]}]
  (let [url (nubes/enhance image_url)]
    (str "Here is the enhanced image:" url)))

(defn slackify-gif [_thread-id {:strs [url]}]
  (let [url (gif/shrink-gif url)]
    (str "Here is the slackified gif:" url)))

(defn rustle-image [_thread-id {:strs [image_url emoji]
                                :as m}]
  (let [opts {:alpha-threshold (get m "alpha_threshold" 128)
              :transparent? (get m "transparent" true)
              :crop? (get m "crop" true)
              :fps (get m "fps" 24)}
        image-url (if image_url
                    image_url
                    (let [emoji (str/replace emoji #":" "")]
                      (some (fn [[emoji-kw emoji-url]]
                              (when (= (name emoji-kw)
                                       emoji)
                                emoji-url))
                            (slack/list-emoji))))]
    (if image-url
      (let [url (gif/rustle-image image-url opts)]
        (str "Here is the rustled image:" url))
      "An image_url or emoji must be provided.")))

(defn label-image [_thread-id {:strs [url]}]
  (let [labels (vision/label-image url)]
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

(defn extract-text [_thread-id {:strs [url]}]
  (let [text (vision/extract-text url)]
    (if (seq text)
      (if (str/includes? text "\"")
        (str "The extracted text:\n" text)
        (str "The extracted text is \"" text "\"."))
      "No text found.")))

(defn run-llava [thread-id {:strs [prompt image_url]}]
  (nubes/run-llava prompt image_url))


(defn resketch [thread-id {:strs [prompt image_url]}]
  (let [urls (nubes/generate-sketch prompt image_url)]
    (str "Here are the images:\n"
         (str/join "\n"
                   (eduction
                    (map-indexed (fn [i url]
                                   (str i ". " url)))
                    urls)))))

(defn generate-music [thread-id {:strs [prompt]}]
  (let [paths (nubes/generate-music prompt)]
    (str "Here are the music clips:\n"
         (str/join "\n"
                   (eduction
                    (map-indexed (fn [i url]
                                   (str i ". " url)))
                    paths)))))


(defn animate [thread-id {:strs [image_urls]}]
  (let [urls (nubes/stable-video-diffusion image_urls)]
    (str "Here are the animations:\n"
         (str/join "\n"
                   (eduction
                    (map-indexed (fn [i url]
                                   (str i ". " url)))
                    urls)))))

(defn dimentiate [thread-id {:strs [image_url]}]
  (let [url (nubes/dimentiate image_url)]
    (str "Here is the polygon file: " url)))

(defn barf [thread-id {}]
  (throw (Exception. "barf")))


(comment
  (println (examine-image nil {"url" "https://pbs.twimg.com/media/GCRbq26WMAANhkP?format=jpg&name=medium"}))
  ,)

(def tool-fns
  (into
   {"generate_images" #'generate-image
    "text_to_speech" #'text-to-speech
    "transcribe" #'transcribe
    "list_attachments" #'list-attachments
    "read_url_link" #'link-reader
    "send_to_main" #'send-to-main
    "examine_image" #'examine-image
    "label_image" #'label-image
    "rustle_image" #'rustle-image
    "extract_text" #'extract-text
    "computer_enhance_image" #'computer-enhance-image
    "run_llava" #'run-llava
    "resketch" #'resketch
    "generate_music" #'generate-music
    "animate" #'animate
    "dimentiate" #'dimentiate
    "retrieve_thread" #'retrieve-thread
    "slackify_gif" #'slackify-gif
    "barf" #'barf}
   (img/tool-fns)))


(defn run-tool* [thread-id 
                 {:keys [id type function]
                  :as tool-call}]
  (prn "running" tool-call)
  (try
    (let [{:keys [name arguments]} function
          arguments (json/read-str arguments)
          tool-fn (get tool-fns name)]
      (when (not tool-fn)
        (throw (Exception. (str "Unknown tool function:" name))))
      (let [output (tool-fn thread-id arguments)]
        (prn "finished " tool-call)
        (when (not (string? output))
          (throw (ex-info "Invalid tool-fn output. Must be string."
                          {:tool-fn tool-fn
                           :argumnets arguments
                           :tool-name name
                           :output output})))
        {:tool_call_id id
         :output output}))
    (catch Throwable e
      (clojure.pprint/pprint e)
      {:tool_call_id id
       :output "An error occurred while running this tool."})))

(defn run-tools! [thread-id run-id tool-calls]
  (let [tool-future-results
        (into []
              (map (fn [tool-call]
                     (.submit @tool-executor
                              (fn []
                                (run-tool* thread-id tool-call)))))
              tool-calls)
        tool-outputs
        (into []
              (map deref)
              tool-future-results)]
    (prn "finished running all tools.")
    (openai/submit-tool-outputs-to-run
     {:thread_id thread-id
      :run_id run-id
      :tool_outputs tool-outputs}
     {:api-key openai-key})))

(defn run-thread [assistant-id thread-id status-ch]
  (let [run (openai/create-run {:thread_id thread-id
                                :assistant_id assistant-id}
                               {:api-key openai-key})
        status (loop [i 0]
                 (Thread/sleep 5000)
                 (prn "checking status" {:thread_id thread-id
                                         :run_id (:id run)})
                 (let [status (openai/retrieve-run {:thread_id thread-id
                                                    :run_id (:id run)}
                                                   {:api-key openai-key
                                                    :request {:timeout 15000
                                                              :connect-timeout 10000}})
                       _ (prn "waiting " thread-id (:id run) (:status status))
                       _ (async/put! status-ch "waiting for thread to complete...")
                       parsed-status
                       ;; fail after 10 minutes
                       (if (> i 120)
                         :fail
                         (case (:status status)
                           ("completed")
                           :complete

                           ("requires_action")
                           (do
                             (prn "requires action" ;;status
                                  )
                             (let [action (:required_action status)
                                   tool-calls (-> action
                                                  :submit_tool_outputs
                                                  :tool_calls)]
                               (when (not= "submit_tool_outputs"
                                           (:type action))
                                 (throw (Exception. "unknown required action type: " (:type action))))
                               (async/put! status-ch
                                           (str
                                            "running tools: "
                                            (str/join
                                             ", "
                                             (map (fn [tool-call]
                                                    (str (-> tool-call
                                                             :function
                                                             :name)))
                                                  tool-calls))))
                               (run-tools! thread-id (:id run) tool-calls)
                               :waiting))

                           ("expired" "failed" "cancelled")
                           (do
                             (clojure.pprint/pprint status)
                             :fail)

                           :waiting))]
                   (case parsed-status
                     :complete
                     (do
                       (async/put! status-ch "thread run complete.")
                       status)
                     
                     :fail
                     (do
                       (async/put! status-ch "thread run fail.")
                       (throw (Exception. "fail.")))
                     ;; else
                     (recur (inc i)))))]
    status))



#_(defn send-rules-response
  [{:keys [response-url thread-ts thread-id text] :as m}]
  (when (seq (clojure.string/trim text))
    (future
      (try
        (let [thread (if thread-id
                       (do
                         (openai/create-message {:thread_id thread-id
                                                 :role "user"
                                                 :content text}
                                                {:api-key openai-key})
                         {:id thread-id})
                       (openai/create-thread {:messages [{:role "user" :content text}]}
                                             {:api-key openai-key}))
              run (openai/create-run {:thread_id (:id thread)
                                      :assistant_id rules-assistant-id}
                                     {:api-key openai-key})
              ;; poll for completion
              _ (loop [i 0]
                  (Thread/sleep 5000)
                  (let [status (openai/retrieve-run {:thread_id (:id thread)
                                                     :run_id (:id run)}
                                                    {:api-key openai-key})
                        parsed-status
                        ;; fail after 10 minutes
                        (if (> i 120)
                          :fail
                          (case (:status status)
                            ("completed")
                            :complete

                            ("expired" "failed" "cancelled")
                            :fail

                            :waiting))]
                    (case parsed-status
                      :complete
                      true
                      
                      :fail
                      (do
                        (client/post response-url
                                     {:body (json/write-str
                                             {
                                              "response_type" "in_channel",
                                              "blocks"
                                              [{"type" "section"
                                                "text" {"type" "plain_text"
                                                        "emoji" true
                                                        "text" (str "ai failed.")}}]
                                              "replace_original" true})
                                      :headers {"Content-type" "application/json"}})
                        (throw (Exception. "fail.")))
                      ;; else
                      (recur (inc i)))))

              all-messages-response (openai/list-messages {:thread_id (:id thread)}
                                                          {:api-key openai-key})
              final-messages (->> all-messages-response
                                  :data
                                  (into []
                                        (keep
                                         (fn [msg]
                                           (let [text
                                                 (->> (:content msg)
                                                      (filter #(= "text" (:type %)))
                                                      first)]
                                             (when text
                                               (let [annotations (-> text :text :annotations)
                                                     annotations-str 
                                                     (when (seq annotations)
                                                       (str "\n"
                                                            (clojure.string/join
                                                             "\n"
                                                             (eduction
                                                              (map (fn [ann]
                                                                     (str "> " 
                                                                          (-> ann
                                                                              :file_citation
                                                                              :quote))))
                                                              annotations))))
                                                     content
                                                     (str 
                                                      (-> text :text :value)
                                                      annotations-str)]
                                                 {:role (:role msg)
                                                  :content content})))))))

              full-response (clojure.string/join "\n\n"
                                                 (into []
                                                       (comp (map (fn [{:keys [role content]}]
                                                                    (case role
                                                                      "user" (str "*" content "*")
                                                                      "assistant"  content))))
                                                       (take-last 2
                                                                  (reverse final-messages))))]
          
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
                                    #_{
                                       "dispatch_action" true,
                                       "type" "input",
                                       "element" {
                                                  "type" "plain_text_input",
                                                  "action_id" (make-action
                                                               [:chat-more (:id thread)])
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
                                    {"thread_ts" thread-ts})))
                          :headers {"Content-type" "application/json"}})))
        (catch Exception e
          (clojure.pprint/pprint e))))))



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
      "description" "List attachments of the given type.",
      "parameters"
      {"type" "object",
       "properties"
       {"type" {"type" "string",
                "enum" ["audio", "video", "image", "plaintext"]
                "description" "The type of the attachment."},},}}}

    #_{"type" "function",
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
     {"name" "retrieve_thread",
      "description" "Returns the transcript for the thread with the given thread id.",
      "parameters"
      {"type" "object",
       "required" ["thread_id"]
       "properties"
       {"thread_id" {"type" "string",
                     "pattern" "^thread-[^-]+-[^-]+$"
                     "description" "A thread id."}}}}}

    {"type" "function",
     "function"
     {"name" "examine_image",
      "description" "Returns an image with the found objects and a table with the name, score, and id for each object found.",
      "parameters"
      {"type" "object",
       "required" ["url"]
       "properties"
       {"url" {"type" "string",
               "description" "A url to an image to examine and find objects."}}}}}

    {"type" "function",
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
      "description" "Enhances an image to be less blurry.",
      "parameters"
      {"type" "object",
       "required" ["image_url"]
       "properties"
       {"image_url" {"type" "string",
                     "pattern" "^http.*"
                     "description" "A url to an image to enhance."}}}}}

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
        "emoji" {"type" "string",
                 "pattern" "^:.*:$"
                 "description" "An emoij to rustle."}
        "transparent" {"type" "boolean"
                       "description" "Whether the gif should be transparent or opaque"
                       "default" true}
        "crop" {"type" "boolean"
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
     {"name" "generate_music",
      "description" "Generates 8 short music clips from a prompt.",
      "parameters"
      {"type" "object",
       "required" ["prompt"]
       "properties"
       {"prompt" {"type" "string",
                  "description" "A short description used to guide the generation of the music clips."}}}}}

    {"type" "function",
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
     {"name" "animate",
      "description" "Animates one or images into videos at the same time.",
      "parameters"
      {"type" "object",
       "required" ["image_urls"]
       "properties"
       {"image_urls" 
        {"type" "array"
         "description" "A list of image urls to animate."
         "items" {"type" "string",
                  "pattern" "^http.*"
                  "description" "A url to an image to animate."}}}}}}

    {"type" "function",
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
                 "enum" ["dalle", "midjourney", "pixel-art-xl"]
                 "description" "The service to use when generating an image."}
        "urls" {"type" "array"
                "description" "A list of urls to base the generated image on."
                "items" {"type" "string",
                         "description" "The service to use when generating an image."}},},
       "required" ["prompt"]}}}
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

    #_{"type" "function",
       "function"
       {"name" "barf",
        "description" "Barfs.",
        "parameters"
        {"type" "object",
         "properties" {}}}}]
   (img/image-schemas)))

(comment
  (def messages
    [{:role "user" :content "Can you generate an image for a lost dog poster?"}])

  (def assistant
    (openai/modify-assistant
     {:name "Scruffy"
      :assistant_id assistant-id
      :model "gpt-4-1106-preview"
      :tools (into [{:type "code_interpreter"}
                    {:type "retrieval"}]
                   tools)
      :instructions "Your a helpful assistant that is secretly an extremely smart dog. You try to avoid revealing that you are dog, but sometimes your dogginess shows when you get excited. If someone asks if you a dog, pretend you didn't hear them or ignore the question."}
     {:api-key openai-key}))

  (def response
    (openai/create-chat-completion {:model "gpt-4"
                                    :tools tools
                                    ;; "gpt-3.5-turbo"
                                    :messages messages}
                                   {:api-key openai-key}))


  (def assistant
    (openai/create-assistant
     {:name "Scruffy"
      :model "gpt-4-1106-preview"
      :tools (into [{:type "code_interpreter"}
                    {:type "retrieval"}]
                   tools)
      :instructions "Your a helpful assistant that is secretly an extremely smart dog. You try to avoid revealing that you are dog, but sometimes your dogginess shows when you get excited. If someone asks if you a dog, pretend you didn't hear them or ignore the question."}
     {:api-key openai-key})
    )

  (def thread
    (openai/create-thread {:messages [{:role "user" :content "Can you read https://www.salon.com/2023/12/02/why-does-sleep-become-more-elusive-as-we-age-it-has-to-do-with-shifts-in-sleep-architecture/"}]}
                          {:api-key openai-key}))
  
  (def result
    (run-thread assistant-id (:id thread))
    )


  (openai/create-message {:thread_id (:id thread)
                          :role "user"
                          :content "Based off what you read, can you generate an image that describes it?"}
                         {:api-key openai-key})

  (def result
    (run-thread assistant-id (:id thread))
      )

  (openai/list-messages {:thread_id (:id thread)}
                        {:api-key openai-key})
  ,)

(defonce assistant-threads (atom {}))
(comment
  (reset! assistant-threads {})
  ,)

(defonce running? (atom true))
(defn new-thread []
  (let [ch (async/chan (async/dropping-buffer 20))]
    (async/thread
      (let [thread (openai/create-thread {}
                                         {:api-key openai-key})]
        (loop []
          (let [loop?
                (try
                  (if-let [prompt-request (async/<!! ch)]
                    (try
                      (let [prompt-requests (loop [reqs [prompt-request]]
                                              (if-let [req (async/poll! ch)]
                                                (recur (conj reqs req))
                                                reqs))
                            status-ch (-> prompt-requests
                                          last
                                          :status-ch)
                            out-ch (-> prompt-requests
                                       last
                                       :ch)]
                        ;; close channels for all but the last request
                        (doseq [pr (butlast prompt-requests)]
                          (async/close! (:ch pr)))

                        (prn "creating messages" prompt-requests)
                        (async/put! status-ch "adding messages")

                        (doseq [pr prompt-requests]
                          (when (seq (:attachments pr))
                            (swap! thread-attachments
                                   update (:id thread)
                                   (fn [m]
                                     (into (or m {})
                                           (map (fn [{:keys [id] :as attachment}]
                                                  [id attachment]))
                                           (:attachments pr)))))
                          (when (seq (:prompt pr))
                            (openai/create-message {:thread_id (:id thread)
                                                    :role "user"
                                                    :content (:prompt pr)}
                                                   {:api-key openai-key})))

                        (prn "running thread")
                        (async/put! status-ch "running thread")
                        (let [result (run-thread assistant-id (:id thread) status-ch)
                              _ (prn "ran thread" ;;result
                                     )
                              response (openai/list-messages {:thread_id (:id thread)}
                                                             {:api-key openai-key})
                              msgs (:data response)]
                          (async/put! status-ch "thread run complete.")
                          (prn "listing messages")
                          (doseq [msg (->> msgs
                                           reverse
                                           (filter #(= "assistant" (:role %))))]
                            (let [text (->> (:content msg)
                                            (filter #(= "text" (:type %)))
                                            first)
                                  id (:id msg)]
                              (async/>!! out-ch {:id id
                                                 :text (-> text :text :value)})))
                          (async/close! out-ch))
                        true)
                      (catch Exception e
                        (clojure.pprint/pprint e)
                        ;; we had an error, but we'll loop anyway.
                        true))
                    ;; channel closed
                    false))]
            (when (and loop? @running?)
              (recur))))))

    ch))

(defn respond [ch thread-id text attachments]
  (try
    (swap! assistant-threads 
           (fn [m]
             (if (get m thread-id)
               m
               (assoc m thread-id
                      (new-thread)))))
    
    (let [prompt-ch (get @assistant-threads thread-id)]
      (async/put! prompt-ch {:prompt text
                             :attachments attachments
                             :ch ch}))))

(defn respond2 [{:keys [ch
                        thread-id
                        prompt
                        status-ch
                        attachments]
                 :as m}]
  (try
    (swap! assistant-threads
           (fn [m]
             (if (get m thread-id)
               m
               (assoc m thread-id
                      (new-thread)))))
    (let [prompt-ch (get @assistant-threads thread-id)]
      (async/put! prompt-ch m))))


