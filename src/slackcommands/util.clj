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

(def image-host "slackcommand.smith.rocks")
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

(defn url->file [fname url]
  (let [f (io/file aimage-dir fname)]
    (with-open [is (io/input-stream (io/as-url url))]
      (io/copy is
               f))
    f))

(defn save-large-png [url]
  (binding [skia/*image-cache* (atom {})]
    (let [view (ui/image (io/as-url url))
          fname (str (random-uuid) ".jpg")]
      (skia/save-image (.getAbsolutePath
                        (io/file aimage-dir fname))
                       view)
      (str "http://" image-host ":" image-port "/aimages/" fname))))

(defn save-and-upload-large-png [url]
  (binding [skia/*image-cache* (atom {})]
    (let [view (ui/image (io/as-url url))
          fname (str (random-uuid) ".jpg")
          f (io/file aimage-dir fname)]
      (skia/save-image (.getAbsolutePath f)
                       view)
      (upload-file f)
      (str "https://" "aimages.smith.rocks/" fname))))

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

(defn split4-large-png [url]
  (binding [skia/*image-cache* (atom {})]
    (let [
          view (ui/image (io/as-url url))
          [w h] (ui/bounds view)
          half-height (quot h 2)
          half-width (quot w 2)
          tl (ui/scissor-view [0 0]
                              [half-width half-height]
                              view)
          tr (ui/scissor-view [0 0]
                              [half-width half-height]
                              (ui/translate (- half-width) 0
                                            view))
          bl (ui/scissor-view [0 0]
                              [half-width half-height]
                              (ui/translate 0 (- half-height)
                                            view))
          br (ui/scissor-view [0 0]
                              [half-width half-height]
                              (ui/translate (- half-width) (- half-height)
                                            view))]
      (into []
            (comp
             (map (fn [view]
                    (let [f (io/file aimage-dir (str (random-uuid) ".jpg"))]
                      (skia/save-image (.getAbsolutePath f)
                                       view)
                      f)))
             (map (fn [f]
                    (upload-file f)
                    f))
             (map (fn [f]
                    (str "https://" "aimages.smith.rocks/" (.getName f)))))
            [tl tr bl br]))))


(defn building-image [urls]
  (binding [skia/*image-cache* (atom {})]
    (let [f (io/file aimage-dir (str "buildings-"
                                     (hash (set urls))
                                     ".jpg"))
          _ (when (not (.exists f))
              (let [imgs (into []
                               (map #(ui/image (io/as-url %)))
                               (sort urls))
                    layout (ui/table-layout
                            (partition-all 4 imgs))]
                (skia/save-image (.getCanonicalPath f)
                                 layout)
                (upload-file f)))]
      (str "https://" "aimages.smith.rocks/" (.getName f)))))
