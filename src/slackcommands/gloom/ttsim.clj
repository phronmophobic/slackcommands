(ns slackcommands.gloom.ttsim
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))



;; /Users/adrian/Library/Tabletop Simulator/Saves



;; https://github.com/any2cards/worldhaven/blob/bacda1f42cca7d723e2d58bcf05112af01c6a13d/data/outpost-building-cards.js#L3

(defn latest-save-file []
  (->> (io/file "/Users/adrian/Library/Tabletop Simulator/Saves/")
     .listFiles
     (filter #(str/ends-with? (.getName %) ".json"))
     (filter #(str/starts-with? (.getName %) "TS_"))
     (apply max-key #(.lastModified %))))

(defn latest-game []
  (with-open [rdr (io/reader (latest-save-file))]
    (json/read rdr)))


(defn default-game []
  (with-open [rdr (io/reader "ttsim.json")]
    (json/read rdr)))

;; layout zones
;; (= "LayoutZone" (get % "Name"))
;; have access to tags
;; items are put into groups:
;; (-> % (get "LayoutZone") (get "GroupsInZone"))

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

(defn prosperity-points->level [pts]
  (loop [prosperity 1
         prev-level 0
         next-level 6]
    (if (>= pts next-level)
      (recur (inc prosperity)
             next-level
             (+ next-level 3 (- next-level prev-level)))
      (str prosperity
           " "
           (- (- next-level prev-level)
              (-  next-level pts))
           "/"
           (- next-level prev-level)))))

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
        prosperity-points (->> (range 1 133)
                               (keep (fn [i]
                                       (let [k (str "prosperity" i)]
                                         (when (seq (get state k))
                                           i))))
                               last)]
    {:prosperity (prosperity-points->level prosperity-points)
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
    (require '[clojure.zip :as z])
    

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

(defn distance
  ([snap o]
   (distance {"Transform" {"posX" 0
                           "posY" 0
                           "posZ" 0
                           "scaleX" 1
                           "scaleY" 1
                           "scaleZ" 1}}
             snap
             o))
  ([mat snap o]
   (let [mat-pos (get mat "Transform")
         spos (get snap "Position")
         coord {:x (+ (get mat-pos "posX")
                      (* (get spos "x")
                         (get mat-pos "scaleX")))
                :y (+ (get mat-pos "posY")
                      (* (get spos "y")
                         (get mat-pos "scaleY")))
                :z (+ (get mat-pos "posZ")
                      (* (get spos "z")
                         (get mat-pos "scaleZ")))}
         {x "posX"
          y "posY"
          z "posZ"
          sx "scaleX"
          sy "scaleT"} (get o "Transform")
]
     (if (and x y z)
       (+ (Math/pow (- x (:x coord)) 2)
          (Math/pow (- y (:y coord)) 2)
          (Math/pow (- z (:z coord)) 2)
          )
       Double/MAX_VALUE))))


(defn find-deck-for-snap
  ([game snap]
   (find-deck-for-snap
    game
    {"Transform" {"posX" 0
                  "posY" 0
                  "posZ" 0
                  "scaleX" 1
                  "scaleY" 1
                  "scaleZ" 1}}
    snap))
  ([game mat snap]
   (let [o (->> (get game "ObjectStates")
                (filter #(= "Deck" (get % "Name")))
                (apply min-key
                       #(distance mat snap %)))]
     (when (< (distance mat snap o) 0.5)
       (->> (get o "ContainedObjects")
            (map #(get % "Nickname"))
            (into #{}))))))




(defn get-buildings-next-level [game]
  (let [mat (->> (get game "ObjectStates")
                 (filter #(= "Outpost"
                             (get % "Nickname")))
                 first)
        snaps (get mat "AttachedSnapPoints")
        snap (->> snaps
                  (filter #(set/subset? #{"deck" "building"}
                                        (set (get % "Tags"))))
                  first)
        all-ids (find-deck-for-snap game mat snap)
        by-id (group-by (fn [s]
                          (str/trim (second (str/split s #"-"))))
                        all-ids)
        ids (into []
                  (map (fn [[id xs]]
                         (apply
                          min-key
                          (fn [s]
                            (let [lvl (re-find #"[0-9]+$" s)]
                              (parse-long lvl)))
                          xs)))
                  by-id)]
    ids))

(defn get-buildings [game]
  (let [mat (->> (get game "ObjectStates")
                 (filter #(= "Outpost"
                             (get % "Nickname")))
                 first)

        snaps (->> (get mat "AttachedSnapPoints")
                   (filter #(set/subset? #{"building"}
                                         (set (get % "Tags")))))
        objects (->> (get game "ObjectStates")
                     (filter #(set/subset? #{"building"}
                                           (set (get % "Tags"))))
                     (filter #(= "Card"
                                 (get % "Name"))))
        find-closest
        (fn [snap]
          (let [o (->> objects
                       (apply
                        min-key
                        #(distance mat snap %)))]
            (when (< (distance mat snap o) 0.5)
              o)))
        all-ids (->> snaps
                     (map #(find-closest %))
                     (remove nil?)
                     (map #(get % "Nickname")))
        by-id (group-by (fn [s]
                          (str/trim (second (str/split s #"-"))))
                        all-ids)
        ids (into []
                  (map (fn [[id xs]]
                         (apply
                          min-key
                          (fn [s]
                            (let [lvl (re-find #"[0-9]+$" s)]
                              (parse-long lvl)))
                          xs)))
                  by-id)]
    ids))

(defn get-items [game]
  (let [states (get game "ObjectStates")
        items-mat (->> states
                       (filter #(= "Items Mat"
                                   (get % "Nickname")))
                       first)
        snaps (get items-mat "AttachedSnapPoints")
        purchasable-snap (->> snaps
                              (filter #(set/subset?
                                        #{"item" "deck" "purchasable"}
                                        (set (get % "Tags"))))
                              first)
        craftable-snap (->> snaps
                            (filter #(set/subset?
                                      #{"item" "deck" "craftable"}
                                      (set (get % "Tags"))))
                            first)]
    {:craftable (find-deck-for-snap game
                                    items-mat
                                    craftable-snap)
     :purchasable (find-deck-for-snap game
                                      items-mat
                                      purchasable-snap)}))
(comment
  (def my-game (default-game))
  (def my-game (latest-game))
  
,)
