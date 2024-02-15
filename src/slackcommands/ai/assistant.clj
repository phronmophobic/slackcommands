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
            [com.phronemophobic.alpaca :as alpaca]
            [slackcommands.util :as util]
            [slackcommands.gif :as gif]
            [slackcommands.ai.img :as img]
            [slackcommands.slack :as slack]
            [slackcommands.ai.vision :as vision]
            [slackcommands.emoji :as emoji]
            [slackcommands.db :as db]
            [clj-slack.chat :as chat]
            [membrane.ui :as ui]
            [clojure.java.io :as io]
            [clj-http.client :as client]
            [clojure.string :as str])
  (:import java.util.concurrent.Executors
           java.util.Date))

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
          (prn e)))))
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


;; not all audio formats are transcrible.
;; eg. iphone created audio.
(defn fix-audio [f]
  (let [output (io/file "/var/tmp/transcribe.mp3")]
    (clj-media/write!
     (->> (clj-media/file (.getCanonicalPath f))
          (clj-media/filter-audio))
     (.getCanonicalPath output))
    output))

(defn transcribe [{:strs [url]}]
  (let [url (util/maybe-download-slack-url url)
        f (or (util/url->local url)
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

(defn link-reader [{:strs [url]}]
  (let [url (util/maybe-download-slack-url url)
        response (http/get url
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

(defonce feature-request-lock (Object.))
(defn feature-request [{:strs [feature_description]}]
  (locking feature-request-lock 
    (spit "feature-requests.txt" feature_description :append true)
    (spit "feature-requests.txt" "\n---------------------------\n" :append true))
  "Thank you for your feature request. It has been logged.")


(defn list-attachments [{:strs [type]
                         :keys [assistant/thread-id]}]
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
          (chat/post-message slack/conn
                             channel
                             markdown
                             {"blocks" 
                              [{
                                "type" "section",
                                "text" {
	                                "type" "mrkdwn",
	                                "text" markdown}}]})
         "Message sent.")
        "Your request to send to main was denied."))))


(defn retrieve-thread [{:strs [thread_id]}]
  (let [[_ channel-id thread-id] (str/split thread_id #"-")]
    (prn channel-id thread-id)
    (if-let [s (slack/retrieve-thread channel-id thread-id)]
      (str "Below is a transcript of the thread:\n\n" s)
      "No thread found for that thread id.")))

(defn examine-image [{:strs [url]}]
  (let [url (util/maybe-download-slack-url url)
        objs (vision/find-objects url)]
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

(defn computer-enhance-image [{:strs [image_url]}]
  (let [url (nubes/enhance (util/maybe-download-slack-url image_url))]
    (str "Here is the enhanced image:" url)))

(defn slackify-gif [{:strs [url]}]
  (let [url (gif/shrink-gif (util/maybe-download-slack-url url))]
    (str "Here is the slackified gif:" url)))

(defn emoji-image-url [{:strs [emoji]}]
  (emoji/emoji->url (str/replace emoji #":" "")))

(defn rustle-image [{:strs [image_url emoji]
                     :as m}]
  (let [opts {:alpha-threshold (get m "alpha_threshold" 128)
              :transparent? (get m "transparent" true)
              :crop? (get m "crop" true)
              :fps (get m "fps" 24)}
        image-url (if image_url
                    (util/maybe-download-slack-url image_url)
                    (let [emoji (str/replace emoji #":" "")]
                      (emoji/emoji->url emoji)))]
    (if image-url
      (let [url (gif/rustle-image image-url opts)]
        (str "Here is the rustled image:" url))
      "An image_url or emoji must be provided.")))

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


#_(defn resketch [{:strs [prompt image_url]}]
  (let [urls (nubes/generate-sketch prompt (util/maybe-download-slack-url image_url))]
    (str "Here are the images:\n"
         (str/join "\n"
                   (eduction
                    (map-indexed (fn [i url]
                                   (str i ". " url)))
                    urls)))))

(defn generate-music [{:strs [prompt]}]
  (let [paths (nubes/generate-music prompt)]
    (str "Here are the music clips:\n"
         (str/join "\n"
                   (eduction
                    (map-indexed (fn [i url]
                                   (str i ". " url)))
                    paths)))))


(defn animate [{:strs [image_urls]}]
  (let [image_urls (map util/maybe-download-slack-url image_urls)
        urls (nubes/stable-video-diffusion image_urls)]
    (str "Here are the animations:\n"
         (str/join "\n"
                   (eduction
                    (map-indexed (fn [i url]
                                   (str i ". " url)))
                    urls)))))

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

(defn treat-log []
  (db/q '[:find (pull ?e [*])
          :where
          [?e :event/type ::dispense-treat]]))


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
                                  (fn []
                                    (let [[emoji description] (rand-nth (seq treats))]
                                     (deliver p 
                                              (str emoji " " description)))))}]
                       (map (fn [[emoji description]]
                              {"type" "button",
		               "text" {
			               "type" "plain_text",
			               "emoji" true,
			               "text" emoji}
		               "value" (wrap-callback 
                                        (fn []
                                          (deliver p (str emoji " " description))))}))
                       treats)}
                {"dispatch_action" true
                 "type" "input"
                 "element" {
                            "type" "plain_text_input",
                            "action_id" 
                            (wrap-callback
                             (fn [payload]
                               (deliver p
                                        (-> payload
                                            (get "actions")
                                            first
                                            (get "value"))))
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

(defn treat-dispenser [{:keys [slack/channel :slack/thread-id]}]
  (let [treat (request-treat channel thread-id)]
    (cond 
      (#{::timeout ::shame} treat)
      (str "The treat dispenser is locked. Did you get permission to take a treat?")

      :else 
      (str "out popped a treat: " treat))))

#_(defn dimentiate [{:strs [image_url]}]
  (let [url (nubes/dimentiate (util/maybe-download-slack-url image_url))]
    (str "Here is the polygon file: " url)))


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
      (json/write-str
       (alpaca/create-order {:symbol symbol
                             :notional (parse-amount amount)}))
      "The request to buy this stonk was denied.")))

(defn get-stonks-balance [{:keys [slack/channel slack/thread-id]}]
  (let [info (alpaca/account)]
    (:effective_buying_power info)))

(defn barf [{}]
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
    "treat_dispenser" #'treat-dispenser
    "emoji_image_url" #'emoji-image-url
    "feature_request" #'feature-request
    ;; stonks
    "list_stonks" #'list-stonks
    "get_stonks_balance" #'get-stonks-balance
    "sell_stonk" #'sell-stonk
    "buy_stonk" #'buy-stonk

    "barf" #'barf}
   (img/tool-fns)))


(defn run-tool* [ctx
                 {:keys [id type function]
                  :as tool-call}]
  (prn "running" tool-call)
  (try
    (let [{:keys [name arguments]} function
          arguments (merge (json/read-str arguments)
                           ctx)
          tool-fn (get tool-fns name)]
      (when (not tool-fn)
        (throw (Exception. (str "Unknown tool function:" name))))
      (let [output (tool-fn arguments)]
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

(defn run-tools! [ctx tool-calls]
  (let [tool-future-results
        (into []
              (map (fn [tool-call]
                     (.submit @tool-executor
                              (fn []
                                (run-tool* ctx tool-call)))))
              tool-calls)
        tool-outputs
        (into []
              (map deref)
              tool-future-results)]
    (prn "finished running all tools.")
    (openai/submit-tool-outputs-to-run
     {:thread_id (:assistant/thread-id ctx)
      :run_id (:run/id ctx)
      :tool_outputs tool-outputs}
     {:api-key openai-key})))

(defn run-thread [ctx status-ch]
  (let [run (openai/create-run {:thread_id (:assistant/thread-id ctx)
                                :assistant_id (:assistant/id ctx)}
                               {:api-key openai-key})
        status (loop [i 0]
                 (Thread/sleep 5000)
                 (prn "checking status" {:thread_id (:assistant/thread-id ctx)
                                         :run_id (:id run)})
                 (let [status (openai/retrieve-run {:thread_id (:assistant/thread-id ctx)
                                                    :run_id (:id run)}
                                                   {:api-key openai-key
                                                    :request {:timeout 15000
                                                              :connect-timeout 10000}})
                       _ (prn "waiting " (:assistant/thread-id ctx) (:id run) (:status status))
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
                               (run-tools! (assoc ctx
                                                  :run/id (:id run))
                                           tool-calls)
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
     {"name" "treat_dispenser",
      "description" "When you've done a good job, the treat dispenser allows the prompter to offer you a treat.",
      "parameters"
      {"type" "object",
       "properties" {}}}}

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
(defn new-thread [ctx]
  (let [ch (async/chan (async/dropping-buffer 20))]
    (async/thread
      (let [thread (openai/create-thread {}
                                         {:api-key openai-key})
            ctx (assoc ctx
                       :assistant/thread-id (:id thread)
                       :assistant/id assistant-id)]
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
                        (let [result (run-thread ctx status-ch)
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

(defn respond [{:keys [ch
                       slack/thread-id
                       slack/channel
                       prompt
                       status-ch
                       attachments]
                :as prompt-info}]
  (try
    (swap! assistant-threads
           (fn [m]
             (if (get m thread-id)
               m
               (assoc m thread-id
                      (new-thread prompt-info)))))
    (let [prompt-ch (get @assistant-threads thread-id)]
      (async/put! prompt-ch prompt-info))))



