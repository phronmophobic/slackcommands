(ns slackcommands.gif
  (:require [com.phronemophobic.clogif :as gif]
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
   (let [img (ui/image (io/as-url url)
                       [50])
         [w h] (mapv long (ui/bounds img))

         crop? (get opts :crop? true)

         max-dx (long (* rustle-pct w))
         max-dy (long (* rustle-pct h))
         rands
         (into []
          (map (fn [_]
                 [(- (rand-int (* 2 max-dx))
                     max-dx)
                  (- (rand-int (* 2 max-dy))
                     max-dy)]))
          (range fps))

         gif-width (if crop?
                     (- w (* 2 max-dx))
                     w)
         gif-height (if crop?
                      (- h (* 2 max-dy))
                      h)
         outf (io/file util/aimage-dir
                       (str (random-uuid) ".gif"))]
     (gif/save-gif!
      (gif/graphics->media
       (fn [g [dx dy]]
         (java2d/draw-to-graphics g
                                  (ui/translate
                                   (- dx (if crop? max-dx 0))
                                   (- dy (if crop? max-dy 0))
                                   img))
         ;; fix for weird transparency issue in ffmpeg
         ;; https://www.reddit.com/r/ffmpeg/comments/qwmh56/glitch_when_creating_transparent_animated_gifs/
         (when (get opts :transparency? true)
           (.clearRect g (dec gif-width) (dec gif-height) 1 1)))
       {:fps fps
        :width gif-width
        :height gif-height}
       rands)
      (.getCanonicalPath outf)
      opts)
     (util/upload-file outf)
     (str "https://" "aimages.smith.rocks/" (.getName outf)))))


(comment
  (rustle-image "https://ca.slack-edge.com/T0RB07YAF-U01729B7HC5-5f52e964f8fe-512")
  (rustle-image "https://aimages.smith.rocks/1416d6ba-87fb-438e-abff-70e40001c86d.png")
  
  ,)
