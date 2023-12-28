(ns slackcommands.stability
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clj-http.client :as client]
            [slingshot.slingshot :refer [try+]]
            [slackcommands.util :as util]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.spec.alpha :as s])
  (:import java.util.Base64))


(defmacro cond-let
  "Takes a binding-form and a set of test/expr pairs. Evaluates each test
  one at a time. If a test returns logical true, cond-let evaluates and
  returns expr with binding-form bound to the value of test and doesn't
  evaluate any of the other tests or exprs. To provide a default value
  either provide a literal that evaluates to logical true and is
  binding-compatible with binding-form, or use :else as the test and don't
  refer to any parts of binding-form in the expr. (cond-let binding-form)
  returns nil."
  [bindings & clauses]
  (let [binding (first bindings)]
    (when-let [[test expr & more] clauses]
      (if (= test :else)
        expr
        `(if-let [~binding ~test]
           ~expr
           (cond-let ~bindings ~@more))))))

(defn parse-tokens [s]
  (let [regex #"\s?(\"[^\"]+\"[-0-9.]*|[\S^\"]+)"]
    (loop [parts []
           s s]
      (let [[whole part] (re-find regex s)]
        (if part
          (recur (conj parts part)
                 (subs s (count whole)))
          parts)))))

(defn token->command [token]
  (cond-let [x]

    (re-find #"width:([0-9]+)" token)
    {:width (parse-long (second x))}

    (re-find #"height:([0-9]+)" token)
    {:height (parse-long (second x))}

    (re-find #"n:([0-9]+)" token)
    {:n (parse-long (second x))}

    (re-find #"steps:([0-9]+)" token)
    {:steps (parse-long (second x))}
    
    (re-find #"style:([A-Z0-9a-z_\-]+)" token)
    {:style-preset (second x)}

    (re-find #"cfg-scale:([0-9]+)" token)
    {:cfg-scale (parse-long (second x))}

    (re-find #"\"([^\"]+)\"([-0-9.]+)" token)
    {:text {:text (second x)
            :weight (parse-double (nth x 2))}}

    (re-find #"\"([^\"]+)\"" token)
    {:text {:text (second x)
            :weight 1}}
    
    :else
    {:text token}))

(defn merge-with-k
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter)."
  {:added "1.0"
   :static true}
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if (contains? m k)
			    (assoc m k (f k (get m k) v))
			    (assoc m k v))))
          merge2 (fn [m1 m2]
		   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defn merge-command [command subcommand]
  (merge-with-k (fn [k a b]
                  (case k
                    :text
                    (if (map? b)
                      (conj a {"text" (:text b)
                               "weight" (:weight b)})
                      (if (= 1 (get (last a) "weight"))
                        (update-in a [(dec (count a)) "text"]
                                   #(str % " " b))
                        (conj a {"text" b
                                 "weight" 1})))

                    ;; else
                    b))
              command
              subcommand))


(def styles
  #{"3d-model"
    "analog-film"
    "anime"
    "cinematic"
    "comic-book"
    "digital-art"
    "enhance"
    "fantasy-art"
    "isometric"
    "line-art"
    "low-poly"
    "modeling-compound"
    "neon-punk"
    "origami"
    "photographic"
    "pixel-art"
    "tile-texture"})


(s/def ::size (s/and integer?
                     #(zero? (mod % 64))))
(s/def :generation/width ::size)
(s/def :generation/height ::size)
(s/def :generation/n (s/int-in 1 11))
(s/def :generation/steps #{50 75 100})
(s/def :generation/style-preset styles)
(s/def :generation/text any? )
(s/def :generation/cfg-scale integer?)

(s/def ::generation-options
  (s/keys
   :req-un [:generation/text]
   :opt-un [:generation/width
            :generation/height
            :generation/n
            :generation/cfg-scale
            :generation/steps
            :generation/style-preset]))

(defn command->opts [command]
  (let [opts (set/rename-keys command
                              {:width "width"
                               :height "height"
                               :n "samples"
                               :cfg-scale "cfg_scale"
                               :steps "steps"
                               :style-preset "style_preset"
                               :text "text_prompts"})]
    opts
    ))

(defn parse-query [s]
  (let [tokens (parse-tokens s)
        command (transduce
                 (map token->command)
                 (completing merge-command)
                 {:text []}
                 tokens)
        ;; with defaults
        command (merge {:n 8
                        :style-preset "enhance"
                        :cfg-scale 20
                        :steps 75}
                       command)
        valid? (s/valid? ::generation-options command)]
    (when (not valid?)
      (throw (ex-info "Invalid options"
                      {:command command
                       :message "Invalid options."})))
    
    (command->opts command)))


(defn b64decode [to-decode]
  (.decode (Base64/getDecoder) to-decode))


(def api-key (:stability/api-key
              (edn/read-string (slurp "secrets.edn"))))



(def base-api-url "https://api.stability.ai")

(def list-engines-endpoint "/v1/engines/list")
(def generation-endpoint "/v1/" )

(def engine
  ;; "stable-diffusion-512-v2-1"
  ;; "stable-diffusion-xl-1024-v1-0"
  "stable-diffusion-xl-beta-v2-2-2"
  )

(defn list-engines []
  (let [response (client/get (str base-api-url list-engines-endpoint)
                             {:headers {"Content-type" "application/json"
                                        "Authorization" (str "Bearer " api-key)}
                              :as :json})]
    (:body response)))

(defn create-image [prompt]
  (let [generation-opts (if (string? prompt)
                          (parse-query prompt)
                          prompt)
        response
        (client/post (str base-api-url "/v1/generation/"engine "/text-to-image")
                     {:body (json/write-str
                             (merge
                              generation-opts
                              {"engine" engine}))
                      :headers {"Content-type" "application/json"
                                "Authorization" (str "Bearer " api-key)}})
        payload (json/read-str (:body response))
        response-id (str (random-uuid))
        urls (into []
                   (comp (map #(get % "base64"))
                         (map b64decode)
                         (map-indexed
                          (fn [i bytes]
                            (let [fname (str response-id "-" i ".png")]
                              (util/save-bytes fname bytes)))))
                   (get payload "artifacts"))]
    urls))

(defn help-text []
  (str
   "Usage: /saimage [query]

Options:
 style:<style>
 cfg-scale:<scale 0-35> How strictly the diffusion process adheres to the prompt text (higher values keep your image closer to your prompt)

Available styles: " (clojure.string/join ", " styles) "

Example:

/saimage A rad looking cow on a rooftop
/saimage style:comic-book An epic fight between tweetica and Ares
/saimage style:neon-punk \"high priority\"10 other text cfg-scale:30

"))

#_(client/post "http://example.org" {:multipart [{:name "title" :content "My Awesome Picture"}
                                               {:name "Content/type" :content "image/jpeg"}
                                               {:name "foo.txt" :part-name "eggplant" :content "Eggplants"}
                                               {:name "file" :content (clojure.java.io/file "pic.jpg")}]
                                   ;; You can also optionally pass a :mime-subtype
                                   :mime-subtype "foo"})

(defn edit-image [input mask prompt]
  (let [generation-opts (parse-query prompt)
        samples 4
        response
        (with-open [is (io/input-stream input)
                    mask-is (io/input-stream mask)]
          (client/post (str base-api-url "/v1/generation/"engine "/image-to-image/masking")
                       {:headers {"Authorization" (str "Bearer " api-key)}
                        :multipart
                        [{:name "text_prompts[0][text]" :content prompt}
                         {:name "text_prompts[0][weight]" :content "1"}
                         {:name "samples" :content (str samples)}
                         {:name "init_image" :content is}
                         {:name "mask_image" :content mask-is}
                         {:name "mask_source" :content "MASK_IMAGE_WHITE"}
                         ;;{:name "style_preset" :content "anime"}
                         #_{:name "image_strength" :content (str 0.85)}]}))
        payload (json/read-str (:body response))
        response-id (str (random-uuid))
        urls (into []
                   (comp (map #(get % "base64"))
                         (map b64decode)
                         (map-indexed
                          (fn [i bytes]
                            (let [fname (str response-id "-" i ".png")]
                              (util/save-bytes fname bytes)))))
                   (get payload "artifacts"))]
    urls))


