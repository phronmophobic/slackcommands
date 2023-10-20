(ns slackcommands.gloom.ttsim
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))



;; /Users/adrian/Library/Tabletop Simulator/Saves



;; https://github.com/any2cards/worldhaven/blob/bacda1f42cca7d723e2d58bcf05112af01c6a13d/data/outpost-building-cards.js#L3
(defn get-buildings [])

(defn default-game []
  (let [rdr (io/reader "ttsim.json")]
    (json/read rdr)))

;; (def game-file (io/file save-root-path savename))
;; (def game (json/read-str (slurp game-file) ))


(defn find-by-tags [game tags]
  (let [tags (set tags)]
    (-> game
        (get "ObjectStates")
        (->> (filter #(set/subset? tags (set (get % "Tags"))))))))


(def resource-names
  ["arrowvine"
   "axenut"
   "corpsecap"
   "flamefruit"
   "hide"
   "lumber"
   "metal"
   "rockroot"
   "snowthistle"])

(defn ->sheet [sheet]
  (let [lua-script-state-json (get sheet "LuaScriptState")
        script-state (json/read-str lua-script-state-json)
        state (get script-state "buttons")
        level (->> (range 1 10)
                   (keep (fn [i]
                           (let [k (str "level" i)]
                             (when (seq (get state k))
                               i))))
                   last)

        name (-> script-state
                 (get "inputs")
                 (get "Name"))
        name (if (seq name)
               name
               (let [nn (get sheet "Nickname")]
                 (subs nn (count "CharacterSheet_"))))]
    {;; :sheet sheet
     :xp (parse-long (get state "xp"))
     :gold (parse-long (get state "gold"))
     :level level
     :name name
     :resources
     (into {}
           (map (fn [k]
                  [k (parse-long (get state k))]))
           resource-names)}))

(defn get-player-mats [game]
  (into []
        (map ->sheet)
        (find-by-tags game ["character sheet"])))

(defn ->campaign [sheet]
  (let [lua-script-state-json (get sheet "LuaScriptState")
        script-state (json/read-str lua-script-state-json)
        state (get script-state "buttons")
        morale (->> (range 20)
                   (keep (fn [i]
                           (let [k (str "morale_" i)]
                             (when (seq (get state k))
                               i))))
                   last)
        prosperity (->> (range 1 10)
                   (keep (fn [i]
                           (let [k (str "prosperity" i)]
                             (when (seq (get state k))
                               i))))
                   last)]
    {:prosperity prosperity
     :morale morale
     :defense (parse-long (get state "total_defense"))
     :inspiration (parse-long (get state "inspiration"))
     :resources
     (into {}
           (map (fn [k]
                  [k (parse-long (get state k))]))
           resource-names)}))

(defn campaign-sheet [game]
  (-> game
      (get "ObjectStates")
      (->> (filter #(= "CampaignSheet" (get % "Nickname"))))
      first
      ->campaign))


(comment
  
  (do
    (require '[zippo.core :as zippo])
    

    (defn clj-zip [obj]
      (z/zipper #(and (seqable? %)
                      (not (string? %)))
                seq
                (fn [node children]
                  (let [children (remove #{::delete} children)]
                    (if (map-entry? node)
                      (vec children)
                      (into (empty node) children))))
                obj))

    (defn search [o pred]
      (let [loc (clj-zip o)]
        (zippo/loc-find-all loc (zippo/->loc-pred pred)))))

  (require 'dev)
  (dev/add-spreadsheet)
  (ss/load-ss "ttsim.edn")

  (ss/save-ss "ttsim.edn")
  ,)
