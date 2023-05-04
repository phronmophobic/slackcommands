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
  (let [regex #"\s?(\"[^\"]+\"|[\S^\"]+)"]
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
    {:num-samples (parse-long (second x))}

    (re-find #"steps:([0-9]+)" token)
    {:steps (parse-long (second x))}
    
    (re-find #"style:([a-z\-]+)" token)
    {:style-preset (second x)}
    
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
                    :text (str a " " b)
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
(s/def :generation/num-samples (s/int-in 1 11))
(s/def :generation/steps #{50 75 100})
(s/def :generation/style-preset styles)
(s/def :generation/text string?)

(s/def ::generation-options
  (s/keys
   :req-un [:generation/text]
   :opt-un [:generation/width
            :generation/height
            :generation/num-samples
            :generation/steps
            :generation/style-preset]))

(defn command->opts [command]
  (let [opts (set/rename-keys command
                              {:width "width"
                               :height "height"
                               :num-samples "samples"
                               :steps "steps"
                               :style-preset "style_preset"})]
    (-> opts
        (assoc "text_prompts" [{"text" (:text opts)
                                "weight" 1}])
        (dissoc :text))))

(defn parse-query [s]
  (let [tokens (parse-tokens s)
        command (transduce
                 (map token->command)
                 (completing merge-command)
                 {}
                 tokens)
        ;; with defaults
        command (merge {:num-samples 4
                        :steps 75}
                       command)
        valid? (s/valid? ::generation-options command)]
    (when (not valid?)
      (throw (ex-info "Invalid options"
                      {:command command})))
    
    (command->opts command)))


(defn b64decode [to-decode]
  (.decode (Base64/getDecoder) to-decode))


(def api-key (:stability/api-key
              (edn/read-string (slurp "secrets.edn"))))



(def base-api-url "https://api.stability.ai")

(def list-engines-endpoint "/v1/engines/list")
(def generation-endpoint "/v1/" )

(def engine "stable-diffusion-512-v2-1")

(defn create-image [prompt]
  (let [generation-opts (parse-query prompt)
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

