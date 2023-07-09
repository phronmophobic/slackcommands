(ns slackcommands.util
  (:require [clojure.java.io :as io]
            [membrane.skia :as skia]
            [membrane.ui :as ui]))


(def aimage-dir
  (doto (io/file "aimages")
    (.mkdirs)))

(def image-host "wavelength.smith.rocks")
;; slack won't show preview images from just any port
;; but port 3000 seems to work
(def image-port 3000 )

(defn save-bytes [fname bytes]
  (let [f (io/file aimage-dir fname)]
    (io/copy bytes f))
  (str "http://" image-host ":" image-port "/aimages/" fname))

(defn save-image [fname url]
  (let [f (io/file aimage-dir fname)]
    (with-open [is (io/input-stream (io/as-url url))]
      (io/copy is
               f)))
  (str "http://" image-host ":" image-port "/aimages/" fname))

(defn split-large-png [url]
  (binding [skia/*image-cache* (atom {})]
    (let [
          view (ui/image (io/as-url url))
          [w h] (ui/bounds view)
          half-height (int (/ h 2))
          top (ui/scissor-view [0 0]
                               [w half-height]
                               view)
          bottom (ui/scissor-view [0 0]
                                  [w half-height]
                                  (ui/translate 0 (- half-height)
                                                view))
          top-fname (str (random-uuid) ".jpg")
          bottom-fname (str (random-uuid) ".jpg")]
      (skia/save-image (.getAbsolutePath
                        (io/file aimage-dir top-fname))
                       top)
      (skia/save-image (.getAbsolutePath
                        (io/file aimage-dir bottom-fname))
                       bottom)
      [(str "http://" image-host ":" image-port "/aimages/" top-fname)
       (str "http://" image-host ":" image-port "/aimages/" bottom-fname)])))


