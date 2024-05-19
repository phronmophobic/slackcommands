(ns slackcommands.gif
  (:require [com.phronemophobic.clogif :as gif]
            [com.phronemophobic.clj-media :as clj-media]
            [com.phronemophobic.clj-media.avfilter :as avfilter]
            [com.phronemophobic.clj-media.model :as mm]
            [membrane.ui :as ui]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [membrane.java2d :as java2d]
            [slackcommands.util :as util]))


(def rustle-pct 0.12)
(def size 77)

(defn shrink-gif* [media opts outf]
  (loop [max-colors 256]
    (when (< max-colors 3)
      (throw (ex-info "Could not shrink gif."
                      {})))
    (gif/save-gif!
     media
     (.getCanonicalPath outf)
     (assoc opts
            :max-colors max-colors))
    (if (> (.length outf)
           125e3)
      (recur (dec max-colors))
      outf)))


(defn vscale [[x y] s]
  [(* x s)
   (* y s)])
(defn vquantize [[x y]]
  [(Math/round x)
   (Math/round y)])


(defn rustle-image
  ([url]
   (rustle-image url {}))
  ([url opts]
   (let [f (or (util/url->local url)
               (util/url->file (str (random-uuid))
                               url))
         fps (get opts :fps 24)
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

         [w h] [size (long
                      (* ih
                         (/ size iw)))]

         crop? (get opts :crop? true)
         max-offset (get opts :max-offset 4)
         direction (get opts :direction "random")
         
         rand-rad (fn []
                    (- 1.0
                       (* 2 (rand))))
         offset-radf (case direction
                       "west" (constantly 1)
                       "northwest" (constantly 0.75)
                       "north" (constantly 0.5)
                       "northeast" (constantly 0.25)
                       "east" (constantly 0)
                       "southeast" (constantly -0.25)
                       "south" (constantly -0.5)
                       "southwest" (constantly -0.75)
                       "random" rand-rad
                       ;; default
                       rand-rad)
         offsetf (fn []
                   (let [rad (* Math/PI (offset-radf))
                         v [(Math/cos rad) (Math/sin rad)]
                         vlen (- max-offset
                                 (* (rand max-offset) 2))
                         v (-> v
                               (vscale vlen)
                               (vquantize))]
                     v))
         max-dx max-offset
         max-dy max-offset
         
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
                  (let [[dx dy] (offsetf)]
                    (ui/translate
                     dx (- dy) ;; y axis is inverted
                     img))))
           (if crop?
             (map (fn [img]
                    (ui/translate
                     (- max-dx) (- max-dy)
                     img)))
             identity))
          (cycle frames))

         outf (io/file util/aimage-dir
                       (str (random-uuid) ".gif"))

         tempf (io/file util/aimage-dir
                        (str (random-uuid) ".gif"))

         media
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

         media (if (:max? opts)
                 (avfilter/negate media)
                 media)]
     (gif/save-gif!
      media
      (.getCanonicalPath tempf)
      opts)
     (shrink-gif* (clj-media/file tempf) opts outf)
     (util/upload-file outf)
     (str "https://" "aimages.smith.rocks/" (.getName outf)))))

(defn shrink-gif [url]
  (let [f (or (util/url->local url)
              (util/url->file (str (random-uuid))
                              url))
        info (clj-media/probe (.getCanonicalPath f))
        {:keys [width height]} (first (:streams info))

        media (clj-media/file f)
        media (if (or (> width size)
                      (> height size))
                (avfilter/scale
                 {:width (str size)
                  :height (str size)}
                 media)
                media)

        outf (io/file util/aimage-dir
                      (str (random-uuid) ".gif"))]
    (shrink-gif*
     media
     {}
     outf)
    (util/upload-file outf)
    (str "https://" "aimages.smith.rocks/" (.getName outf))))

(comment

  ,)

(defn -main [& args]
  
  )
