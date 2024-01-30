(ns slackcommands.clip
  (:require [com.phronemophobic.clip :as clip]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.nippy :as nippy]
            [slackcommands.util :as util]
            [clojure.edn :as edn]))

(def ctx
  (delay (clip/create-context "models/CLIP-ViT-B-32-laion2B-s34B-b79K_ggml-model-f16.gguf")))

(def banned
  #{"75c2e44e-94a5-47d0-ad36-52ff456b308d.png"
    "0e355d1d-60fe-4449-999f-ae8278ce5586.png"
    "7fc90bca-c58f-4e49-9af8-153ce037fa68.jpg"
    "e0377ae8-44b1-4029-a6ba-8753ad6e3208.jpg"
    "6212cca9-5d17-4f6a-b72e-c4c6dafebbc2.png"
    "2ccf30af-a437-4a8c-b501-2a0f36d2c068.png"
    "989c9689-5f46-4236-87a5-886455dac87e.png"
    "aa7a4cf1-20df-45eb-9e74-dbc0d7327d57.jpg"
    "eeedebd5-edd3-4949-8682-8c666580e857.png"
    "90564407-c7be-4deb-9413-c5ff5ea2191e.png"
    "6b325bde-c5d4-4fbd-ab7f-2eaa646b71d3.png"
    
    })


;; (defn transduce-ednl [fname xform f ]
;;   (with-open [rdr (io/reader fname)
;;               rdr (java.io.PushbackReader. rdr)]
;; transduce
;;     (transduce )
;;     ))

(def embeddings 
  (delay (nippy/thaw-from-file "embeddings.nippy")))

(defn find-nearest [text]
  (let [
        
        
        text-embedding (if (str/starts-with? text "http")
                         (let [url (str/trim text)
                               f (or (util/url->local url)
                                     (util/url->file (str (random-uuid)) url))]
                           (clip/image-embedding @ctx f))
                         (clip/text-embedding @ctx text))
        nearest
        (->> (sort-by 
              (fn [[fname xs]]
                (clip/cosine-similarity 
                 text-embedding
                 xs))
              @embeddings)
             reverse
             (map first)
             (remove banned)
             (distinct)
             (take 8))]
    nearest))


