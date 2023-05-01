(ns slackcommands.ai
  (:require [clojure.java.io :as io]
            [wkok.openai-clojure.api :as api]
            [clojure.data.json :as json]
            [clojure.core.memoize :as memo]
            [clj-http.client :as client]
            [clojure.edn :as edn]))



(def api-key (:chatgpt/api-key
              (edn/read-string (slurp "secrets.edn"))))


(defn chat-command [request]
  ;; gpt-3.5-turbo
  (let [text (get-in request [:form-params "text"])
        response-url (get-in request [:form-params "response_url"])]

    (when (seq (clojure.string/trim text))
      (future
        (let [response (api/create-chat-completion {:model "gpt-3.5-turbo"
                                                    :messages [{:role "user" :content text}]}
                                                   {:api-key api-key})
              chat-response (-> response
                                :choices
                                first
                                :message
                                :content)]
          (try
            (client/post response-url
                         {:body (json/write-str
                                 {
                                  "response_type" "in_channel",
                                  "text" chat-response

                                  "replace_original" "true"})
                          :headers {"Content-type" "application/json"}})
            (catch Exception e
              (prn e))))))

    {:body (json/write-str
            {"response_type" "in_channel"
             }) 
     :headers {"Content-type" "application/json"}
     :status 200}


    )
)

(def ^:dynamic action-data nil)
(defn -get-action [s]
  (assert action-data)
  action-data)
(def one-day (* 1000 86400))
(defonce get-action (memo/ttl -get-action :ttl/threshold one-day))
(defn make-action [data]
  (binding [action-data data]
    (let [s (str "ai" (hash data))]
      (get-action s)
      s)))

(defn image-response [prompt urls index]
  (let [url (nth urls index)
        main-blocks [ {"type" "image",
                       "title" {"type" "plain_text",
                                "text" (str prompt " (" (inc index) "/" (count urls) ")")},
                      "image_url" url
                      "alt_text" prompt}]]
    {"response_type" "in_channel"
     "blocks"
     (into main-blocks
           [{"type" "divider"}
            {"type" "actions",
             "elements"
             [{"type" "button",
               "text"
               {"type" "plain_text", "text" "<<"}
               "value" (make-action [:get-image prompt urls (mod (dec index) (count urls))])}
              {"type" "button",
               "text"
               {"type" "plain_text", "text" ">>"}
               "value" (make-action [:get-image prompt urls (mod (inc index) (count urls))])}]}])})
  )

(defn ai-command-interact [request]
  (let [payload (json/read-str (get (:form-params request) "payload"))
        url (get payload "response_url")
        action (-> payload
                   (get "actions")
                   first
                   (get "value"))
        [action-type & action-args :as event] (get-action action)]
    (case action-type
      :get-image
      (let [[_ prompt urls index] event]
        (future
          (try
            (client/post url
                         {:body (json/write-str
                                 (assoc (image-response prompt urls index)
                                        "replace_original" true))
                          :headers {"Content-type" "application/json"}})
            (catch Exception e
              (prn e))))
        {:body "ok"
         :headers {"Content-type" "application/json"}
         :status 200})

      ;; else
      nil)))

(def aimage-dir
  (doto (io/file "aimages")
    (.mkdirs)))

(defn save-image [fname url]
  (let [f (io/file aimage-dir fname)]
    (with-open [is (io/input-stream (io/as-url url))]
      (io/copy is
               f)))
  nil)

(defn image-command [request]
  (let [text (get-in request [:form-params "text"])
        response-url (get-in request [:form-params "response_url"])]
    (when (seq (clojure.string/trim text))
      (future
        (let [response (api/create-image {:prompt text
                                          :n 8
                                          :size
                                          ;; "256x256"
                                          "512x512"
                                          ;; "1024x1024"
                                          }
                                         {:api-key api-key})

              response-id (str (random-uuid))
              ;; image-url (-> response
              ;;               :data
              ;;               first
              ;;               :url)
              host (:server-name request)
              ;; slack won't show preview images from just any port
              port 3000
              urls (into []
                         (comp (map :url)
                               (map-indexed
                                (fn [i url]
                                  (let [fname (str response-id "-" i ".png")]
                                    (save-image fname
                                                url)
                                    fname)))
                               (map (fn [fname]
                                      (str "http://" host ":" port "/aimages/" fname))))
                         (:data response))]
          (try
            (client/post response-url
                         {:body (json/write-str
                                 (image-response text urls 0)
                                 #_{
                                  "response_type" "in_channel",
                                  "blocks"
                                  [{
                                    "type" "image",
                                    "image_url" image-url
                                    "alt_text" text
                                    ;; "text" image-url
                                    }
                                   ]

                                  "replace_original" true})
                          :headers {"Content-type" "application/json"}})
            (catch Exception e
              (prn e))))))

    {:body (json/write-str
            {"response_type" "in_channel"
             })
     :headers {"Content-type" "application/json"}
     :status 200}))

(comment
  (def response
    
  (api/create-image {:prompt "An oracle visiting the real monsters tribe"
                     :n 5
                     :size "512x512"}
                    {:api-key api-key})

    (def response
      )
    ,)

(def imgs
  (into []
        (comp (map :url)
              (map io/as-url)
              (map ui/image))
        (:data response))
  )

  (require '[membrane.skia :as skia]
           '[membrane.ui :as ui])

  (skia/run #'debug)
  ,)




