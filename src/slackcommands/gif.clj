(ns slackcommands.gif
  (:require [com.phronemophobic.clogif :as gif]
            [com.phronemophobic.clj-media :as clj-media]
            [com.phronemophobic.clj-media.model :as mm]
            [membrane.ui :as ui]
            [clojure.java.io :as io]
            [membrane.java2d :as java2d]
            [slackcommands.util :as util]))

(def fps 24)
(def rustle-pct 0.12)

(defn rustle-image
  ([url]
   (rustle-image url {}))
  ([url opts]
   (let [f (or (util/url->local url)
               (util/url->file (str (random-uuid))
                               url))
         frames (into []
                      (map mm/image)
                      (clj-media/frames
                       (clj-media/file f)
                       :video
                       {:format (clj-media/video-format
                                 {:pixel-format :pixel-format/rgba})}))
         first-frame (first frames)
         iw (.getWidth first-frame)
         ih (.getHeight first-frame)

         [w h] [50 (long
                    (* iw
                       (/ 50 iw)))]

         crop? (get opts :crop? true)
         max-dx (long (* rustle-pct w))
         max-dy (long (* rustle-pct h))

         gif-width (if crop?
                     (- w (* 2 max-dx))
                     w)
         gif-height (if crop?
                      (- h (* 2 max-dy))
                      h)

         num-frames (max fps
                         (count (seq frames)))

         rustled-frames
         (eduction
          (comp
           (take num-frames)
           (map (fn [buf-img]
                  (ui/image buf-img [w h])))
           (map (fn [img]
                  (ui/translate
                   (- (rand-int (* 2 max-dx))
                      max-dx)
                   (- (rand-int (* 2 max-dy))
                      max-dy)
                   img)))
           (if crop?
             (map (fn [img]
                    (ui/translate
                     (- max-dx) (- max-dy)
                     img)))
             identity))
          (cycle frames))

         outf (io/file util/aimage-dir
                       (str (random-uuid) ".gif"))]
     (gif/save-gif!
      (gif/graphics->media
       (fn [g view]
         (java2d/draw-to-graphics g view)
         ;; fix for weird transparency issue in ffmpeg
         ;; https://www.reddit.com/r/ffmpeg/comments/qwmh56/glitch_when_creating_transparent_animated_gifs/
         (when (get opts :transparency? true)
           (.clearRect g (dec gif-width) (dec gif-height) 1 1)))
       {:fps fps
        :width gif-width
        :height gif-height}
       rustled-frames)
      (.getCanonicalPath outf)
      opts)
     (util/upload-file outf)
     (str "https://" "aimages.smith.rocks/" (.getName outf)))))


(comment
  (rustle-image "https://ca.slack-edge.com/T0RB07YAF-U01729B7HC5-5f52e964f8fe-512")
  (rustle-image "https://aimages.smith.rocks/1416d6ba-87fb-438e-abff-70e40001c86d.png")
  
  ,)
