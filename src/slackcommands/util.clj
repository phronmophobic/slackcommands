(ns slackcommands.util
  (:require [clojure.java.io :as io]))


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
