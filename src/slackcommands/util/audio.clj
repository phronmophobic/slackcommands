(ns slackcommands.util.audio
  (:require [wkok.openai-clojure.api :as openai]
            [com.phronemophobic.clj-media :as clj-media]
            [slackcommands.util :as util]
            [pantomime.mime :as mime]
            pantomime.media
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def openai-key
  (:chatgpt/api-key
   (edn/read-string (slurp "secrets.edn"))))

;; not all audio formats are transcrible.
;; eg. iphone created audio.
(defn fix-audio [f]
  (let [output (io/file "/var/tmp/transcribe.mp3")]
    (clj-media/write!
     (->> (clj-media/file (.getCanonicalPath f))
          (clj-media/filter-audio))
     (.getCanonicalPath output))
    output))

(defn transcribe-url
  "Retrieves the audio from `url`, transcribes the audio, and returns the text."
  [url]
  (let [url (util/maybe-download-slack-url url)
        f (or (util/url->local url)
              (util/url->file (str (random-uuid))
                              url))
        mime-type (mime/mime-type-of f)
        _ (when (and (not (pantomime.media/audio? mime-type))
                     (not (pantomime.media/video? mime-type)))
            (throw (ex-info "URL must be audio mimetype."
                            {:url url})))
        ;; todo check mimetype of url.
        _ (prn "transcribing" f (.exists f))
        response (openai/create-transcription
                  {:model "whisper-1"
                   :file (fix-audio f)}
                  {:api-key openai-key})
        text (:text response)]
    text))


