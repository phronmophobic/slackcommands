(ns slackcommands.ai.assistant
  (:require [wkok.openai-clojure.api :as openai]
            [clojure.java.shell :as sh]
            [clojure.edn :as edn]
            [clojure.core.async :as async]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [com.phronemophobic.discord.api :as discord]
            [slackcommands.util :as util]
            [clojure.java.io :as io]
            [clj-http.client :as client]
            [clojure.string :as str])
  (:import java.util.concurrent.Executors))

(def openai-key
  (:chatgpt/api-key
   (edn/read-string (slurp "secrets.edn"))))

(def assistant-id "asst_FFFmAt4eemrv3BZ8TFUq4Ob5")

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

(defn link-reader [{:strs [url]}]
  (let [response (http/get url)
        html (:body response)
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
       byline
       text-content]))))




(defn generate-image [{:strs [prompt]}]
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
                 (util/save-large-png))]
    url))

(def tool-fns {"generate_image" #'generate-image
               "text_to_speech" #'text-to-speech
               "read_url_link" #'link-reader})


(defn run-tool* [{:keys [id type function]
                  :as tool-call}]
  (prn "running" tool-call)
  (let [{:keys [name arguments]} function
        arguments (json/read-str arguments)
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
       :output output})))

(defn run-tools! [thread-id run-id tool-calls]
  (let [tool-future-results
        (into []
              (map (fn [tool-call]
                     (.submit @tool-executor
                              (fn []
                                (run-tool* tool-call)))))
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

(defn run-thread [assistant-id thread-id]
  (let [run (openai/create-run {:thread_id thread-id
                                :assistant_id assistant-id}
                               {:api-key openai-key})
        status (loop [i 0]
                 (Thread/sleep 5000)
                 (let [status (openai/retrieve-run {:thread_id thread-id
                                                    :run_id (:id run)}
                                                   {:api-key openai-key})
                       _ (prn "waiting " thread-id (:id run) (:status status))
                       parsed-status
                       ;; fail after 10 minutes
                       (if (> i 120)
                         :fail
                         (case (:status status)
                           ("completed")
                           :complete

                           ("requires_action")
                           (do
                             (prn "requires action" status)
                             (let [action (:required_action status)]
                               (when (not= "submit_tool_outputs"
                                           (:type action))
                                 (throw (Exception. "unknown required action type: " (:type action))))
                               (run-tools! thread-id (:id run)
                                           (-> action
                                               :submit_tool_outputs
                                               :tool_calls))
                               :waiting))

                           ("expired" "failed" "cancelled")
                           (do
                             (clojure.pprint/pprint status)
                             :fail)

                           :waiting))]
                   (case parsed-status
                     :complete
                     status
                     
                     :fail
                     (throw (Exception. "fail."))
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
    {"name" "generate_image",
     "description" "Generates an imageid given a prompt.",
     "parameters"
     {"type" "object",
      "properties"
      {"prompt" {"type" "string",
                 "description" "A prompt that describes the picture to be generated"},},
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
      "required" ["text"]}}}])

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

(defn respond [ch thread-id text]
  (try
    (swap! assistant-threads 
           (fn [m]
             (if (get m thread-id)
               m
               (assoc m thread-id
                      (delay 
                        (openai/create-thread {}
                                              {:api-key openai-key}))))))
    
    (let [thread @(get @assistant-threads thread-id)]
      (openai/create-message {:thread_id (:id thread)
                              :role "user"
                              :content text}
                             {:api-key openai-key})
      (prn "created message")
      (prn "running thread")
      (let [result (run-thread assistant-id (:id thread))
            _ (prn "ran thread" result)
            response (openai/list-messages {:thread_id (:id thread)}
                                           {:api-key openai-key})
            msgs (:data response)]
        (prn "listing messages" response)
        (doseq [msg (->> msgs
                         reverse
                         (filter #(= "assistant" (:role %))))]
          (let [text (->> (:content msg)
                          (filter #(= "text" (:type %)))
                          first)
                id (:id msg)]
            (async/>!! ch {:id id
                           :text (-> text :text :value)})))
        (async/close! ch)))
    (catch Exception e
      (clojure.pprint/pprint e)
      (async/>!! ch {:id (gensym)
                     :text "Error!"}))))
