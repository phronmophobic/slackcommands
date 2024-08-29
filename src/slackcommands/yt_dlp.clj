(ns slackcommands.yt-dlp
  (:require [clojure.java.shell :as sh]
            [slackcommands.util :as util]
            [clojure.java.io :as io]))


(def yt-dlp-path (.getCanonicalPath (io/file "yt-dlp")))


(defn download-vid [url]
  (let [fname (str (random-uuid) ".mp4")
        f (io/file util/aimage-dir fname )
        fpath (.getCanonicalPath f)
        args [yt-dlp-path url "-o" fpath]
        {:keys [exit out err] :as result} (apply sh/sh args )]
    (if (zero? exit)
      (let [url (util/upload-file f)]
        (.delete f)
        (str "The video is available at " url))
      (do
        (println "yt-dlp-error:\nstdout:\n" out "\nstderr\n" err)
        (str "There wan an error download the vid.")))))
