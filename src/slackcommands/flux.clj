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
   :ideogram "https://fal.run/fal-ai/ideogram/v2"
   :flux-schnell "https://fal.run/fal-ai/flux/schnell"
   :flux-dev "https://fal.run/fal-ai/flux/dev"
   :stable-audio "https://fal.run/fal-ai/stable-audio"
   :stable-video "https://fal.run/fal-ai/stable-video"
   :music-generator "https://fal.run/cassetteai/music-generator"
   :minimax-music "https://fal.run/fal-ai/minimax-music"
   :animate-diff "https://fal.run/fal-ai/animatediff-v2v"
   :clarity-upscaler "https://fal.run/fal-ai/clarity-upscaler"
   :creative-upscaler "https://fal.run/fal-ai/creative-upscaler"
   :illusion-diffusion "https://fal.run/fal-ai/illusion-diffusion"
   :plushify "https://fal.run/fal-ai/plushify"
   :cartoonify "https://fal.run/fal-ai/cartoonify"
   :gemini-flash-edit "https://fal.run/fal-ai/gemini-flash-edit"
   :depth "https://fal.run/fal-ai/imageutils/depth"
   :star-vector "https://fal.run/fal-ai/star-vector"
   :marigold-depth "https://fal.run/fal-ai/imageutils/marigold-depth"
   :cogvideox-5b "https://fal.run/fal-ai/cogvideox-5b"})

;; sing lyrics to a given input music track
;; https://fal.ai/models/fal-ai/minimax-music

;; image editing
;; https://fal.ai/models/fal-ai/gemini-flash-edit

;; https://fal.ai/models/fal-ai/cartoonify
;; https://fal.ai/models/fal-ai/plushify

;; ideo gram
;; https://fal.ai/models/fal-ai/ideogram/v2

;; generate a song clip from a prompt
;; https://fal.ai/models/cassetteai/music-generator

;; multi speaker dialog tts
;; https://fal.ai/models/fal-ai/playai/tts/dialog

;; image -> svg
;; https://fal.ai/models/fal-ai/star-vector

(defn generate* [{:keys [endpoint prompt image_url source_image_url driven_audio_url reference_audio_url duration]}]
  (let [response (client/post endpoint
                              {:accept :json
                               :form-params 
                               (merge
                                (when reference_audio_url
                                  {:reference_audio_url reference_audio_url})
                                (when duration
                                  {:duration duration})
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
           :plushify
           :gemini-flash-edit
           :cartoonify} model)
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

(defn generate-svg [{:keys [image-url] :as opts}]
  
  (when (not image-url)
    (throw (ex-info "Image url required"
                    {:opts opts})))
  (let [endpoint (get api-url :star-vector)
        response 
        (generate* (assoc opts
                          :endpoint endpoint
                          :image_url image-url))
        url (or (-> response :images first :url)
                (-> response :image :url))]
    (util/save-and-upload-stream (str (random-uuid) ".svg") (io/as-url url))))

(defn generate-audio [{:keys [prompt audio-url model] :as opts}]
  (when (and
         (#{:minimax-music} model)
         (not audio-url))
    (throw (ex-info "Audio url required"
                    {:opts opts})))
  (let [
        response 
        (generate* (assoc opts
                          :duration 45
                          :endpoint (get api-url model)
                          :reference_audio_url audio-url))
        
        url (-> response :audio_file :url)]
    (util/save-and-upload-stream (str (random-uuid) ".wav") (io/as-url url))))

(defn generate-video [{:keys [prompt] :as opts}]
  (let [response 
        (generate* (assoc opts :endpoint (:cogvideox-5b api-url)))
        url (-> response :video :url)]
    
    (util/save-and-upload-stream (str (random-uuid) ".mp4") (io/as-url url))))

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
                      (:stable-video api-url))
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

(defn image-edit [{:keys [image-url prompt]}]
  (generate-image {:model :gemini-flash-edit
                   :image-url image-url
                   :prompt prompt}))


(comment

  ;; https://fal.ai/models/fal-ai/minimax/video-01-live
  ;; https://fal.ai/models/fal-ai/minimax-music
  ;; https://fal.ai/models/fal-ai/kling-video/v1.5/pro/image-to-video?utm_source=fal&utm_medium=email&utm_campaign=whats-new-in-fal-new-video-image-models-portrait

  ,)
