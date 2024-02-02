(ns slackcommands.emoji
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [slackcommands.util :as util]
            [membrane.skia :as skia]
            [membrane.ui :as ui]
            ;; [membrane.skia.paragraph :as para]
            [clojure.java.io :as io]))


(def emoji-data-url
  "https://raw.githubusercontent.com/iamcal/emoji-data/master/emoji.json")

(defn hex-to-emoji
  [s]
  (str/join
   (eduction
    (mapcat (fn [code-point-str]
              (let [code-point (Integer/parseInt code-point-str 16)]
                (Character/toChars code-point))))
    (clojure.string/split s #"-"))))

(defn emoji-table* []
  (let [emoji-data
        (with-open [rdr (io/reader (io/as-url emoji-data-url))]
          (json/read rdr))]
    (into {}
          (mapcat (fn [{:strs [unified short_names]}]
                    (let [s (hex-to-emoji unified)]
                      (eduction
                       (map (fn [nm]
                              [nm s]))
                       short_names))))
          emoji-data)))

(def emoji-table
  (delay
    (emoji-table*)))

(defn emoji->url [short-name]
  (let [emoji (get @emoji-table short-name)
        view
        (ui/padding 30
         (ui/label emoji
                   (ui/font (.getCanonicalPath (io/file "NotoColorEmoji-Regular.ttf"))
                            100)))
        #_(para/paragraph
           {:text emoji
            :style
            #:text-style {:font-size 100
                          :font-families ["/Users/adrian/Downloads/Noto_Color_Emoji/NotoColorEmoji-Regular.ttf"]}})
              fname (str "emoji-" short-name ".png")
        f (io/file
           "aimages"
           fname)]
    (skia/save-image (.getCanonicalPath f)
                     view
                     nil
                     nil
                     100
                     false)
    (util/upload-file f)
    (str "https://" "aimages.smith.rocks/" fname)))
