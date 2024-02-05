(ns slackcommands.util
  (:require [clojure.java.io :as io]
            [membrane.skia :as skia]
            [amazonica.core :as amazonica]
            [amazonica.aws.s3 :as s3]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.data.json :as json]
            [clj-http.client :as client]
            [datalevin.core :as d]
            [membrane.ui :as ui]))

(def s3-creds
  {:profile "aimages"
   :endpoint "us-west-1"})

(amazonica/defcredential s3-creds)
(def bucket "aimages-smith-rocks")

(defn upload-file
  ([fname metadata]
   (let [f (io/file fname)
         key (.getName f)]
     (with-open [is (io/input-stream f)]
       (s3/put-object bucket
                      key
                      is
                      metadata))))
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

(defn url->local
  "Returns a file from url if it's already local. nil otherwise."
  [url]
  (when (and (str/starts-with? url "https://aimages.smith.rocks/" )
            (.exists (io/file aimage-dir (subs url (count "https://aimages.smith.rocks/")))))
    (io/file aimage-dir (subs url (count "https://aimages.smith.rocks/")))))

(defn url->file [fname url]
  (let [f (io/file aimage-dir fname)]
    (with-open [is (io/input-stream (io/as-url url))]
      (io/copy is
               f))
    f))

(defn stream->file [fname is]
  (let [f (io/file aimage-dir fname)]
    (with-open [is (io/input-stream is)]
      (io/copy is
               f))
    f))

(defn save-and-upload-stream 
  ([fname is]
   (save-and-upload-stream fname is nil))
  ([fname is metadata]
   (let [f (io/file aimage-dir fname)]
     (with-open [is (io/input-stream is)]
       (io/copy is
                f))
     (if metadata
       (upload-file f metadata)
       (upload-file f))
     (str "https://" "aimages.smith.rocks/" fname))))

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


(defn save-and-upload-view [view-fn]
  (binding [skia/*image-cache* (atom {})]
    (let [view (view-fn)
          fname (str (random-uuid) ".jpg")
          f (io/file aimage-dir fname)]
      (skia/save-image (.getAbsolutePath f)
                       view)
      (upload-file f)
      (str "https://" "aimages.smith.rocks/" fname))))

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

(defn content-type->suffix [content-type]
  (case content-type
    "image/png" ".png"
    ("text/html" "text/plain") ".txt"
    "application/json" ".json"
    "application/pdf" ".pdf"
    ("image/jpg" "image/jpeg") ".jpg"
    "image/gif" ".gif"
    "audio/mpeg" ".mpeg"
    "audio/mp4" ".mp4"
    "video/mp4" ".mp4"
    "application/zip" ".zip"
    "application/xhtml+xml" ".xml"
    "application/javascript" ".js"
    "application/rss+xml" ".xml"
    "text/css" ".css"
    "audio/x-wav" ".wav"
    "text/csv" ".csv"
    "image/svg+xml" ".svg"
    "text/xml" ".xml"
    "application/xml" ".xml"
    "video/webm" ".webm"
    "audio/webm" ".webm"
    "image/webp" ".webp"
    "image/bmp" ".bmp"
    ;; else
    ""))

(defn audio? [mimetype]
  (and (string? mimetype)
       (str/starts-with? mimetype "audio/")))

(defn video? [mimetype]
  (and (string? mimetype)
       (str/starts-with? mimetype "video/")))

(defn image? [mimetype]
  (and (string? mimetype)
       (str/starts-with? mimetype "image/")))

(defn plaintext? [mimetype]
  (= mimetype "text/plain"))

(defn read-edn [fname]
  (with-open [is (io/input-stream fname)
              ;; is (if (str/ends-with? fname ".gz")
              ;;      (GZIPInputStream. is)
              ;;      is)
              rdr (io/reader is)
              rdr (java.io.PushbackReader. rdr)]
    (edn/read rdr)))

(def misc-table "misc-test-table")
(defonce db
  (delay
    (doto (d/open-kv (.getCanonicalPath (io/file "kv.db")))
      (d/open-dbi misc-table))))

;; slack interactions
(defn make-action
  ([var-sym]
   (make-action var-sym {}))
  ([var-sym data]
   (let [action-str (str "action-" (random-uuid))]
     (d/transact-kv
      @db
      [[:put misc-table action-str
        {:var-sym var-sym
         :data data}]])
     action-str)))

(defn get-action [action-str]
  (d/get-value @db misc-table action-str))

(defn delete-message [payload data]
  (let [url (get payload "response_url")]
    (future
      (try
        (client/post url
                     {:body (json/write-str
                             {"delete_original" true})
                      :headers {"Content-type" "application/json"}})
        (catch Exception e
          (prn e))))
    {:body "ok"
     :headers {"Content-type" "application/json"}
     :status 200}))
