(ns slackcommands.util
  (:require [clojure.java.io :as io]
            [membrane.skia :as skia]
            [amazonica.core :as amazonica]
            [amazonica.aws.s3 :as s3]
            [membrane.ui :as ui]))

(def s3-creds
  {:profile "aimages"
   :endpoint "us-west-1"})

(amazonica/defcredential s3-creds)
(def bucket "aimages-smith-rocks")

(defn upload-file
  ([fname]
   (let [f (io/file fname)
         key (.getName f)]
     (s3/put-object bucket
                    key
                    f))))

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

(defn save-image
  ([url]
   (save-image (str (random-uuid)) url))
  ([fname url]
   (let [f (io/file aimage-dir fname)]
     (with-open [is (io/input-stream (io/as-url url))]
       (io/copy is
                f)))
   (str "http://" image-host ":" image-port "/aimages/" fname)))

(defn save-large-png [url]
  (binding [skia/*image-cache* (atom {})]
    (let [view (ui/image (io/as-url url))
          fname (str (random-uuid) ".jpg")]
      (skia/save-image (.getAbsolutePath
                        (io/file aimage-dir fname))
                       view)
      (str "http://" image-host ":" image-port "/aimages/" fname))))

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
          bottom-fname (str (random-uuid) ".jpg")

          top-file (io/file aimage-dir top-fname)
          bottom-file (io/file aimage-dir bottom-fname)]
      (skia/save-image (.getAbsolutePath
                        top-file)
                       top)
      (skia/save-image (.getAbsolutePath
                        bottom-file)
                       bottom)
      (upload-file top-file)
      (upload-file bottom-file)
      [(str "https://" "aimages.smith.rocks/" top-fname)
       (str "https://" "aimages.smith.rocks/" bottom-fname)])))


