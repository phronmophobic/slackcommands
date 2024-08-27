(ns slackcommands.flux
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clj-http.client :as client]
            [slingshot.slingshot :refer [try+]]
            [slackcommands.util :as util]))


(def api-key (:flux/api-key
              (edn/read-string (slurp "secrets.edn"))))


;; curl --request POST \
;;   --url https://fal.run/fal-ai/aura-flow \
;;   --header "Authorization: Key $FAL_KEY" \
;;   --header "Content-Type: application/json" \
;;   --data '{
;;      "prompt": "Close-up portrait of a majestic iguana with vibrant blue-green scales, piercing amber eyes, and orange spiky crest. Intricate textures and details visible on scaly skin. Wrapped in dark hood, giving regal appearance. Dramatic lighting against black background. Hyper-realistic, high-resolution image showcasing the reptile's expressive features and coloration."
;;    }'

(def api-url
  {:flux-pro "https://fal.run/fal-ai/flux-pro"
   :flux-schnell "https://fal.run/fal-ai/flux/schnell"
   :flux-dev "https://fal.run/fal-ai/flux/dev"
   :stable-audio "https://fal.run/fal-ai/stable-audio"
   :stable-video "https://fal.run/fal-ai/stable-video"
   :animate-diff "https://fal.run/fal-ai/animatediff-v2v"
   :clarity-upscaler "https://fal.run/fal-ai/clarity-upscaler"
   :creative-upscaler "https://fal.run/fal-ai/creative-upscaler"
   :illusion-diffusion "https://fal.run/fal-ai/illusion-diffusion"
   :depth "https://fal.run/fal-ai/imageutils/depth"
   :marigold-depth "https://fal.run/fal-ai/imageutils/marigold-depth"})

(defn generate* [{:keys [endpoint prompt image_url source_image_url driven_audio_url]}]
  (let [response (client/post endpoint
                              {:accept :json
                               :form-params 
                               (merge
                                (when prompt
                                  {:prompt prompt})
                                (when image_url
                                  {:image_url image_url})
                                (when source_image_url
                                  {:source_image_url source_image_url})
                                (when driven_audio_url
                                  {:driven_audio_url driven_audio_url}))
                               :content-type :json
                               :headers {"Authorization" (str "Key " api-key)}
                               :as :json})]
    (:body response)))

(defn generate-image [{:keys [model prompt image-url] :as opts}]
  
  (when (#{:illusion-diffusion
           :depth
           :marigold-depth
           } model)
    (when (not image-url)
      (throw (ex-info "Image url required"
                      {:opts opts}))))
  (let [endpoint (get api-url model (:flux-dev api-url))
        response 
        (generate* (assoc opts
                          :endpoint endpoint
                          :image_url image-url))
        url (or (-> response :images first :url)
                (-> response :image :url))]
    (util/save-and-upload-stream (str (random-uuid) ".png") (io/as-url url))))

(defn generate-audio [{:keys [prompt] :as opts}]
  (let [response 
        (generate* (assoc opts :endpoint (:stable-audio api-url)))
        url (-> response :audio_file :url)]
    (util/save-and-upload-stream (str (random-uuid) ".wav") (io/as-url url))))



(defn upscale
  "Model can be :clarity-upscaler or :creative-upscaler"
 [{:keys [image-url model] :as opts}]
  (when-not image-url
    (throw (ex-info "Image url required"
                    {:opts opts})))
  (let [endpoint (get api-url model 
                      (:clarity-upscaler api-url))
        response (generate* {:image_url image-url
                             :endpoint endpoint})
        url (-> response :image :url)]
    (util/save-and-upload-stream (str (random-uuid) ".png") (io/as-url url))))


(defn animate [{:keys [image-url model prompt] :as opts}]
  (when-not image-url
    (throw (ex-info "Image url required"
                    {:opts opts})))

  (let [endpoint (get api-url model 
                      (:animate-diff api-url))
        _ (when (= endpoint (:animate-diff api-url))
            (when (not prompt)
              (throw (ex-info "Prompt required"
                              {:opts opts}))))
        response (generate* 
                  (merge
                   {:image_url image-url
                    :endpoint (:stable-video api-url)}
                   (when prompt
                     {:prompt prompt})))
        url (-> response :video :url)]
    (util/save-and-upload-stream (str (random-uuid) ".mp4") (io/as-url url))))

(defn sadtalker [{:keys [image-url audio-url]}]
  (let [response (generate* 
                  {:source_image_url image-url
                   :driven_audio_url audio-url
                   :endpoint "https://fal.run/fal-ai/sadtalker"})
        url (-> response :video :url)]
    (util/save-and-upload-stream (str (random-uuid) ".mp4") (io/as-url url))))


(comment
  ,)
