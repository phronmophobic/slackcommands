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
         [w h] (ui/bounds img)
         w (long w)
         h (long h)
         outf (io/file util/aimage-dir
                       (str (random-uuid) ".gif"))]
     (gif/save-gif!
      (gif/graphics->media
       (fn [g [dx dy]]
         (java2d/draw-to-graphics g
                                  (ui/translate
                                   (* w dx) (* h dy)
                                   img)))
       {:fps fps
        :width w
        :height h}
       (eduction
        (map (fn [_]
               [(- (/ rustle-pct 2) (* rustle-pct (rand)))
                (- (/ rustle-pct 2) (* rustle-pct (rand)))]))
        (range fps)))
      (.getCanonicalPath outf)
      opts)
     (util/upload-file outf)
     (str "https://" "aimages.smith.rocks/" (.getName outf)))))


(comment
  (rustle-image "https://ca.slack-edge.com/T0RB07YAF-U01729B7HC5-5f52e964f8fe-512")
  (rustle-image "https://aimages.smith.rocks/1416d6ba-87fb-438e-abff-70e40001c86d.png")
  
  ,)
