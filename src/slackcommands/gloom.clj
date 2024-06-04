(ns slackcommands.gloom
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clj-http.client :as client]
            [wkok.openai-clojure.api :as openai]
            [clojure.pprint :as pp]
            [clojure.core.memoize :as memo]
            [slackcommands.gloom.ttsim :as ttsim]
            [slackcommands.util :as util]
            [clojure.zip :as z]
            [clojure.edn :as edn]
            [clojure.data.json :as json])
  (:import java.util.regex.Pattern))

(def openai-key
  (:chatgpt/api-key
   (edn/read-string (slurp "secrets.edn"))))

(defmacro cond-let
  "Takes a binding-form and a set of test/expr pairs. Evaluates each test
  one at a time. If a test returns logical true, cond-let evaluates and
  returns expr with binding-form bound to the value of test and doesn't
  evaluate any of the other tests or exprs. To provide a default value
  either provide a literal that evaluates to logical true and is
  binding-compatible with binding-form, or use :else as the test and don't
  refer to any parts of binding-form in the expr. (cond-let binding-form)
  returns nil."
  [bindings & clauses]
  (let [binding (first bindings)]
    (when-let [[test expr & more] clauses]
      (if (= test :else)
        expr
        `(if-let [~binding ~test]
           ~expr
           (cond-let ~bindings ~@more))))))

(defn markdown-response [s]
  (json/write-str
   {"response_type" "in_channel"
    "blocks"
    [{"type" "section"
      "text" {"type" "mrkdwn",
              "text" s}}]}))

(defn card-level [card]
  (case (:level card)
    ("-" "A" "B" "M" "P" "V" "X") 1
    ("1" "2" "3" "4" "5" "6" "7" "8" "9") (parse-long (:level card))))

(defn card-initiative [card]
  (parse-long (:initiative card)))

(defn image-url [card]
  (str "https://raw.githubusercontent.com/any2cards/worldhaven/master/images/"
       (:image card)))

(defn list-cards-response [cards]
  (let [header {"type" "section"
                "text" {"type" "mrkdwn",
                        "text" "all cards"}
                "accessory" {"type" "button"
                             "text" {"type" "plain_text"
                                     "text" "delete"}
                             "value" (util/make-action `util/delete-message)}}
        main-blocks (for [[i card] (map-indexed vector cards)]
                      {"type" "section"
                       "text" {"type" "mrkdwn",
                               "text" (str
                                       ":" (:character-xws card " ") ": "
                                       "*" (:name card) "*: "
                                       "lvl " (:level card) " "
                                       "(" (:initiative card) ")"
                                       " ")}
                       "accessory" {"type" "button"
                                    "text" {"type" "plain_text"
                                            "text" "view"}
                                    "value" (util/make-action `get-card-interact {:cards cards
                                                                                  :index i})}
                       })]
    {"response_type" "in_channel"
     "blocks" (into [header]
                    main-blocks)}))

(defn card-response [cards index]
  (let [card (nth cards index)
        main-blocks [{"type" "section",
                      "text" {"type" "mrkdwn",
                              "text"
                              (str (inc index) " of " (count cards) " matches.")}
                      "accessory" {"type" "button"
                                   "text" {"type" "plain_text"
                                           "text" "delete"}
                                   "value" (util/make-action `util/delete-message)}}

                     {"type" "divider"}
                     {"type" "image",
                      "title" {"type" "plain_text",
                               "text" (str (:character-xws card) ": "
                                           (:name card))},
                      "image_url" (image-url card)
                      "alt_text" (str (:character-xws card) ": " (:name card))}]]
    {"response_type" "in_channel"
     "blocks"
     (into main-blocks
           (when (> (count cards) 1)
             [{"type" "divider"}
              {"type" "actions",
               "elements"
               (into []
                     (concat
                      (for [i (range (max 0 (dec index))
                                     (min (count cards)
                                          (+ index 5)))
                            :when (not= i index)
                            :let [card (nth cards i)]]
                        {"type" "button",
                         "text"
                         {"type" "plain_text", "text" (:name card)}
                         "value" #_(util/make-action [:getcard cards i])
                         (util/make-action
                          `get-card-interact {:cards cards
                                              :index i})})
                      [{"type" "button",
                        "text"
                        {"type" "plain_text", "text" "..."}
                        "value" (util/make-action `list-cards-interact {:cards cards})}]))
               }]))})
  )

(defn list-items-response [items]
  (let [header {"type" "section"
                "text" {"type" "mrkdwn",
                        "text" "all items"}
                "accessory" {"type" "button"
                             "text" {"type" "plain_text"
                                     "text" "delete"}
                             "value" (util/make-action `util/delete-message)}}
        main-blocks (for [[i item] (map-indexed vector items)]
                      {"type" "section"
                       "text" {"type" "mrkdwn",
                               "text" (str (:xws item) " (" (:expansion item) ")")}
                       "accessory" {"type" "button"
                                    "text" {"type" "plain_text"
                                            "text" "view"}
                                    "value" #_(make-action [:getitem items i])
                                    (util/make-action `get-item-interact
                                                      {:items items
                                                       :index i})}
                       })]
    {"response_type" "in_channel"
     "blocks" (into [header]
                    main-blocks)}))

(defn item-response [items index]
  (let [item (nth items index)
        main-blocks [{"type" "section",
                      "text" {"type" "mrkdwn",
                              "text"
                              (str (inc index) " of " (count items) " matches.")}
                      "accessory" {"type" "button"
                                   "text" {"type" "plain_text"
                                           "text" "delete"}
                                   "value" (util/make-action `util/delete-message)}}

                     {"type" "divider"}
                     {"type" "image",
                      "title" {"type" "plain_text",
                               "text" (str (:xws item) " (" (:expansion item) ")")},
                      "image_url" (image-url item)
                      "alt_text" (str (:xws item) " (" (:expansion item) ")")}]]
    {"response_type" "in_channel"
     "blocks"
     (into main-blocks
           (when (> (count items) 1)
             [{"type" "divider"}
              {"type" "actions",
               "elements"
               (into []
                     (concat
                      (for [i (range (max 0 (dec index))
                                     (min (count items)
                                          (+ index 5)))
                            :when (not= i index)
                            :let [item (nth items i)]]
                        {"type" "button",
                         "text"
                         {"type" "plain_text", "text" (:xws item)}
                         "value" #_(make-action [:getitem items i])
                         (util/make-action `get-item-interact {:items items
                                                               :index i})})
                      [{"type" "button",
                        "text"
                        {"type" "plain_text", "text" "..."}
                        "value" (util/make-action
                                 `list-items-interact
                                 {:items items})}]))}]))})
  )

(defn campaign->md [c]
  (with-out-str
    (pp/print-table
     [:name
      :prosperity
      :defense
      :inspiration
      :morale]
     [(assoc c :name "Frosthaven")])))




(defn stats->md []
  (let [game (ttsim/default-game)
        campaign (ttsim/campaign-sheet game)
        players (ttsim/get-player-mats game)]
    (str
     "```\n"
     (with-out-str
       (clojure.pprint/print-table
        [:name :level :xp :gold]
        players))

     (let [xs (concat
               (eduction
                (map (fn [{:keys [name resources]}]
                       (assoc resources :name name)))
                players)
               [(assoc (:resources campaign)
                       :name "FrostHaven")
                (assoc (reduce #(merge-with + %1 %2)
                               (:resources campaign)
                               (map :resources players))
                       :name "Total")])
           n 5]
       (clojure.string/join
        (eduction
         (partition-all n)
         (map (fn [ks]
                (with-out-str
                  (pp/print-table
                   (into [:name] ks)
                   xs))))
         ttsim/resource-names)))

     (campaign->md campaign)
     "\n```")))

(comment
  (println
   (clojure.pprint/print-table
    [:name :level :xp :gold]
    (ttsim/get-player-mats (ttsim/default-game))))

  (println (stats->md))
  
  (println (campaign->md
            (ttsim/campaign-sheet (ttsim/default-game))))
  ,)

(defn block-zip [blocks]
  (z/zipper vector?
            seq
            (fn [v children]
              (into (empty v) children))
            blocks))

(defn reset-chat-text-input [blocks]
  (let [zip (block-zip blocks)]
    (loop [zip zip]
      (if (z/end? zip)
        (z/root zip)
        ;; else
        (let [m (z/node zip)
              new-zip (if (= "input" (get m "type"))
                        (z/replace zip (dissoc m "block_id"))
                        zip)]
          (recur (z/next new-zip))))))
  )

(defn stats-response []
  (let [blocks [{"type" "section",
                 "text" {"type" "mrkdwn",
                         "text" (stats->md)}}]]
    {"response_type" "in_channel"
     "blocks" blocks}))


(def rules-assistant-id "asst_6t54KDnUcP4n2MiPrljbdTyG")
(defn send-rules-response
  [{:keys [response-url thread-ts thread-id text] :as m}]
  (when (seq (clojure.string/trim text))
    (future
      (try
        (let [thread (if thread-id
                       (do
                         (openai/create-message {:thread_id thread-id
                                                 :role "user"
                                                 :content text}
                                                {:api-key openai-key})
                         {:id thread-id})
                       (openai/create-thread {:messages [{:role "user" :content text}]}
                                             {:api-key openai-key}))
              run (openai/create-run {:thread_id (:id thread)
                                      :assistant_id rules-assistant-id}
                                     {:api-key openai-key})
              ;; poll for completion
              _ (loop [i 0]
                  (Thread/sleep 5000)
                  (let [status (openai/retrieve-run {:thread_id (:id thread)
                                                     :run_id (:id run)}
                                                    {:api-key openai-key})
                        parsed-status
                        ;; fail after 10 minutes
                        (if (> i 120)
                          :fail
                          (case (:status status)
                            ("completed")
                            :complete

                            ("expired" "failed" "cancelled")
                            :fail

                            :waiting))]
                    (case parsed-status
                      :complete
                      true
                      
                      :fail
                      (do
                        (client/post response-url
                                     {:body (json/write-str
                                             {
                                              "response_type" "in_channel",
                                              "blocks"
                                              [{"type" "section"
                                                "text" {"type" "plain_text"
                                                        "emoji" true
                                                        "text" (str "ai failed.")}}]
                                              "replace_original" true})
                                      :headers {"Content-type" "application/json"}})
                        (throw (Exception. "fail.")))
                      ;; else
                      (recur (inc i)))))

              all-messages-response (openai/list-messages {:thread_id (:id thread)}
                                                          {:api-key openai-key})
              final-messages (->> all-messages-response
                                  :data
                                  (into []
                                        (keep
                                         (fn [msg]
                                           (let [text
                                                 (->> (:content msg)
                                                      (filter #(= "text" (:type %)))
                                                      first)]
                                             (when text
                                               (let [annotations (-> text :text :annotations)
                                                     annotations-str 
                                                     (when (seq annotations)
                                                       (str "\n"
                                                            (clojure.string/join
                                                             "\n"
                                                             (eduction
                                                              (map (fn [ann]
                                                                     (str "> " 
                                                                          (-> ann
                                                                              :file_citation
                                                                              :quote))))
                                                              annotations))))
                                                     content
                                                     (str 
                                                      (-> text :text :value)
                                                      annotations-str)]
                                                 {:role (:role msg)
                                                  :content content})))))))

              full-response (clojure.string/join "\n\n"
                                                 (into []
                                                       (comp (map (fn [{:keys [role content]}]
                                                                    (case role
                                                                      "user" (str "*" content "*")
                                                                      "assistant"  content))))
                                                       (take-last 2
                                                                  (reverse final-messages))))]
          
          (doseq [chunk (partition-all 2999 full-response)
                  :let [subresponse (apply str chunk)]]
            (client/post response-url
                         {:body (json/write-str
                                 (merge
                                  {
                                   "response_type" "in_channel",
                                   "blocks"
                                   [{"type" "section"
                                     "text" {"type" "mrkdwn"
                                             "text" subresponse}}
                                    {
                                     "dispatch_action" true,
                                     "type" "input",
                                     "element" {
                                                "type" "plain_text_input",
                                                "action_id" #_(make-action
                                                               [:chat-more (:id thread)])
                                                (util/make-action
                                                 `chat-more-interact
                                                 {:thread-id (:id thread)})},
                                     "label" {
                                              "type" "plain_text",
                                              "text" "yes, and?",
                                              "emoji" true
                                              }
                                     }
                                    ]
                                   ;; "thread_ts" thread-ts
                                   "replace_original" false}
                                  (when thread-ts
                                    {"thread_ts" thread-ts})))
                          :headers {"Content-type" "application/json"}})))
        (catch Exception e
          (clojure.pprint/pprint e))))))

(defn list-cards-interact [payload {:keys [cards]}]
  (let [url (get payload "response_url")]
    (future
      (try
        (client/post url
                     {:body (json/write-str
                             (assoc (list-cards-response cards)
                                    "replace_original" true))
                      :headers {"Content-type" "application/json"}})
        (catch Exception e
          (prn e))))
    {:body "ok"
     :headers {"Content-type" "application/json"}
     :status 200}))


(defn get-card-interact [payload {:keys [cards index]}]
  (let [url (get payload "response_url")]
    (future
      (try
        (client/post url
                     {:body (json/write-str
                             (assoc (card-response cards
                                                   index)
                                    "replace_original" true))
                      :headers {"Content-type" "application/json"}})
        (catch Exception e
          (prn e))))
    {:body "ok"
     :headers {"Content-type" "application/json"}
     :status 200}))

(defn list-items-interact [payload {:keys [items]}]
  (let [url (get payload "response_url")]
    (future
      (try
        (client/post url
                     {:body (json/write-str
                             (assoc (list-items-response items)
                                    "replace_original" true))
                      :headers {"Content-type" "application/json"}})
        (catch Exception e
          (prn e))))
    {:body "ok"
     :headers {"Content-type" "application/json"}
     :status 200}))

(defn get-item-interact [payload {:keys [items index]}]
  (let [url (get payload "response_url")]
    (future
      (try
        (client/post url
                     {:body (json/write-str
                             (assoc (item-response items
                                                   index)
                                    "replace_original" true))
                      :headers {"Content-type" "application/json"}})
        (catch Exception e
          (prn e))))
    {:body "ok"
     :headers {"Content-type" "application/json"}
     :status 200}))

(defn chat-more-interact [payload {:keys [thread-id]}]
  (let [url (get payload "response_url")
        ts (get-in payload ["message" "thread_ts"]
                   (get-in payload ["message" "ts"]))
        action (-> payload
                   (get "actions")
                   first
                   (get "value"))]
    (future
      (send-rules-response
       {:response-url url
        :thread-ts ts
        :thread-id thread-id
        :text action})
      (let [blocks (get-in payload ["message" "blocks"])]
        (client/post url
                     {:body (json/write-str
                             {"blocks" (reset-chat-text-input blocks)
                              "response_type" "in_channel"
                              "replace_original" true})
                      :headers {"Content-type" "application/json"}})))

    {:status 200}))

(def worldhaven-root
  (io/file "../worldhaven"))

(def data-files
  (->> worldhaven-root
       .listFiles
       (filter #(= "data" (.getName %)))
       first
       .listFiles
       (filter #(str/ends-with? (.getName %) ".js"))
       (into [])))

(defn data-file [nm]
  (->> data-files
       (filter #(= nm (.getName %)))
       first))

(defn data [nm]
  (let [f (data-file nm)]
    (with-open [rdr (io/reader f)]
      (json/read rdr
                 :key-fn keyword))))


(def tla->xws
  {
   "AA" "amberaegis"
   "BB" "blinkblade"
   "BE" "berserker"
   "BK" "brightspark"
   "BM" "bombard"
   "BN" "bannerspear"
   "BO" "boneshaper"
   "BR" "brute"
   "BS" "bladeswarm"
   "BT" "beasttyrant"
   "CG" "chainguard"
   "CH" "cragheart"
   "CR" "crashing tide"
   "CT" "chieftain"
   "DE" "demolitionist"
   "DF" "drifter"
   "DR" "diviner"
   "DS" "doomstalker"
   "DT" "deepwraith"
   "DW" "deathwalker"
   "EL" "elementalist"
   "FF" "frozenfist"
   "FK" "fireknight"
   "GE" "geminate"
   "HA" "hatchet"
   "HO" "hollowpact"
   "HP" "hierophant"
   "HV" "hive"
   "IF" "infuser"
   "IN" "incarnate"
   "LU" "luminary"
   "ME" "metalmosaic"
   "MF" "mirefoot"
   "MT" "mindthief"
   "NS" "nightshroud"
   "PC" "painconduit"
   "PH" "plagueherald"
   "PY" "pyroclast"
   "QA" "artificer"
   "QM" "quartermaster"
   "RG" "redguard"
   "RH" "rimehearth"
   "RM" "ruinmaw"
   "SB" "sawbones"
   "SC" "scoundrel"
   "SD" "snowdancer"
   "SH" "shattersong"
   "SK" "sunkeeper"
   "SP" "spiritcaller"
   "SR" "shardrender"
   "SS" "soothsinger"
   "ST" "starslinger"
   "SU" "summoner"
   "SW" "spellweaver"
   "TA" "trapper"
   "TI" "tinkerer"
   "TP" "tempest"
   "TR" "thornreaper"
   "VQ" "vanquisher"
   "VW" "voidwarden"
   })

(def xws->tla
  (into {}
        (map (fn [[k v]]
               [v k]))
        tla->xws))


(defn remove-card-dupes [cards]
  (let [cards-by-xws (group-by (juxt :xws :character-xws) cards)]
    (into #{}
          (map (fn [[xws cards]]
                 (apply max-key :points cards)))
          cards-by-xws)))

(defn image->xws [img]
  (let [parts (clojure.string/split img #"/")
        tla (nth parts 2)]
    (tla->xws tla)))

(def cards
  (remove-card-dupes
   (into
    #{}
    (comp
     (map (fn [{:keys [image] :as card}]
            (assoc card :character-xws (image->xws image)))))
    (data "character-ability-cards.js"))))

(def mats (data "character-mats.js"))
(def mats-by-xws (into {}
                       (map (juxt :xws identity))
                       mats))



(defn standard-sort [cards]
  (sort-by (juxt
            :character-xws
            card-level
            #(= "X" (:level %))
            card-initiative)
           cards))

(defn cards-block [cards]
  (into []
        (map (fn [card]
               {"type" "image",
                "title" {"type" "plain_text",
                         "text" (:name card)},
                "image_url" (image-url card)
                "alt_text" (str (:character-xws card) ": " (:name card))}))
        cards))

(defn parse-tokens [s]
  (let [regex #"\s?(\"[^\"]+\"|[\S^\"]+)"]
    (loop [parts []
           s s]
      (let [[whole part] (re-find regex s)]
        (if part
          (recur (conj parts part)
                 (subs s (count whole)))
          parts)))))

(def character-xws? (into #{}
                          (vals tla->xws)))



(def expansion?
  (into {"gh" "Gloomhaven"
         "cs" "Crimson Scales"
         "fh" "Frosthaven"
         "jothl" "Jaws Of The Lion"
         "toa" "Trail of Ashes"
         "fc" "Forgotten Circles"}
        (comp (map :expansion)
              (map (fn [s]
                     [(-> s
                          str/lower-case
                          (str/replace #" " "-"))
                      s])))
        cards))

(def special?
  {"mrgreen"
   {:character"redguard"}

   "mrgarbage"
   {:character "quartermaster"}
   
   "voidy"
   {:character "voidwarden"}

   "crab"
   {:character "deepwraith"}

   "all-characters"
   {:character (vec character-xws?)}

   "all-classes"
   {:character (vec character-xws?)}

   "all"
   {:character (vec character-xws?)}

   "all-expansions"
   {:expansion (vec (vals expansion?))}
   
   })

(def character?
  (into #{}
        (map :character-xws)
        cards))

(defn token->command [token]
  (cond-let [x]

    (special? (str/lower-case token))
    x

    (expansion? token)
    {:expansion x}

    (character? token)
    {:character x}

    (re-find #"\"([^\"]+)\"" token)
    {:text (str/lower-case (second x))}

    (re-matches #"([0-9]{1,2})l" token)
    {:level (parse-long (second x))}

    (re-matches #"([0-9]+)-([0-9]+)l" token)
    {:level {:from (parse-long (second x))
             :to (parse-long (nth x 2))}}

    (re-matches #"([0-9]+)-l" token)
    {:level {:from (parse-long (second x))
             :to Long/MAX_VALUE}}

    (re-matches #"-([0-9]+)l" token)
    {:level {:to (parse-long (second x))
             :from 0}}

    (re-matches #"([0-9]{1,2})i" token)
    {:initiative (parse-long (second x))}

    (re-matches #"([0-9]+)-([0-9]+)i" token)
    {:initiative {:from (parse-long (second x))
                  :to (parse-long (nth x 2))}}

    (re-matches #"([0-9]+)-i" token)
    {:initiative {:from (parse-long (second x))
                  :to Long/MAX_VALUE}}

    (re-matches #"-([0-9]+)i" token)
    {:initiative {:to (parse-long (second x))
                  :from 0}}


    (re-matches #"([0-9]+)i" token)
    {:initative (parse-long (second x))}
    
    :else
    {:text token}))

(defn merge-command [command subcommand]
  (merge-with (fn [a b]
                (if (vector? a)
                  (conj a b)
                  [a b]))
              command
              subcommand))

(defn add-command-defaults [command]
  (let [command (if (:expansion command)
                  command
                  (assoc command
                         :expansion ["Gloomhaven"
                                     "Frosthaven"
                                     "Jaws Of The Lion"]))
        party ["painconduit"
               "blinkblade"
               "pyroclast"]
        command (if (:character command)
                  command
                  (assoc command
                         :character party))
        command (if (:level command)
                  command
                  (assoc command
                         :level 1))]
    command))

(defn parse-query [s]
  (let [tokens (parse-tokens s)
        command (transduce
                 (map token->command)
                 (completing merge-command)
                 {}
                 tokens)
        command (add-command-defaults command)]
    command
    #_(when command
        (if (contains? command "set")
          command
          (merge command (token->command "standard"))))))


(defmulti subcommand->pred (fn [k q]
                             k))

(defmethod subcommand->pred :initiative [k q]
  (cond
    (number? q)
    #(= q (card-initiative %))

    (map? q)
    (fn [card]
      
      (when-let [i (card-initiative card)]
        (and (>= (:to q) i)
             (<= (:from q) i))))

    (vector? q)
    (fn [card]
      (some #((subcommand->pred k %) card)
            q))))


(defmethod subcommand->pred :level [k q]
  (cond
    (number? q)
    #(= q (card-level %))

    (map? q)
    (fn [card]
      (let [l (card-level card)]
        (and (>= (:to q) l)
             (<= (:from q) l))))

    (vector? q)
    (fn [card]
      (some #((subcommand->pred k %) card)
            q))))

(defmethod subcommand->pred :expansion [k q]
  (cond
    (string? q)
    #(= q (:expansion %))
    
    :else
    (fn [card]
      (some #((subcommand->pred k %) card)
            q)))
  )


(defmethod subcommand->pred :text [k q]
  (cond
    (string? q)
    (fn [card]
      (some #(str/includes? (str/lower-case %) q)
            [(:name card)]))
    
    :else
    (fn [card]
      (every? #((subcommand->pred k %) card)
              q))))


(defmethod subcommand->pred :character [k q]
  (cond
    (string? q)
    #(= (:character-xws %) q)
    
    :else
    (fn [card]
      (some #((subcommand->pred k %) card)
            q))))

(defn command->xform [command]
  (let [preds (map
               (fn [[k v]]
                 (subcommand->pred k v))
               command)]
    (filter (fn [card]
              (every? #(% card)
                      preds)))))



(defn query-cards
  ([q]
   (query-cards cards q))
  ([cards q]
   (let [command (->> (parse-query q))
         xform (comp
                (remove #(#{"-" "A" "B" "M" "P" "V"}
                          (:level %)))
                (command->xform command))]
     (standard-sort
      (into []
            xform
            cards))))
  )

(def items (data "items.js"))
(def buildings (data "outpost-building-cards.js"))
(def buildings-by-xws
  (into {}
        (map (fn [[xws cards]]
               [xws
                (->> cards
                     (remove #(str/ends-with?(:image %)
                                            "-back.png"))
                     (apply
                      max-key
                      :points))]))
        (group-by :xws buildings)))
(defn ->building [ttsim-id]
  (let [[_ bid name lvl] (re-matches #"([0-9]+)\s*-\s*([a-zA-Z 0-9]+)\s*-[^0-9]*([0-9]+)" ttsim-id)
        xws (str bid
                 (-> name
                     str/lower-case
                     (str/replace #"\s" ""))
                 "level"
                 lvl)
        ]
    (buildings-by-xws xws)))

(defn building-response [kind]
  (let [get-buildings (case kind
                        :current ttsim/get-buildings
                        :next ttsim/get-buildings-next-level)
        buildings
        (map ->building
             (get-buildings (ttsim/default-game)))
        url (util/building-image
             (map image-url buildings))
        blocks [{"type" "image",
                 "title" {"type" "plain_text",
                          "text" "Buildings"},
                 "image_url" url
                 "alt_text" "Buildings"}]]
    {"response_type" "in_channel"
     "blocks" blocks}))


(defn fuzzy-regex [s]
  (Pattern/compile
   (str
    ".*"
    (apply str
           (->> (str/lower-case s)
                (remove #{\space})
                (map (fn [c]
                       (str "[" c "]")))
                (interpose ".*")))
    ".*")))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll with duplicates removed.
  Returns a stateful transducer when no collection is provided."
  ([keyfn]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [k (keyfn input)]
            (if (contains? @seen k)
              result
              (do (vswap! seen conj k)
                  (rf result input))))))))))


(defn find-items [s]
  (let [{:keys [craftable purchasable]} (ttsim/get-items (ttsim/default-game))

        available-ids (set/union craftable
                                 purchasable)
        available-items
        (->> items
             (map (fn [card]
                    (let [num (re-find #"[0-9]{3}$" (:name card))
                          id (case (:expansion card)
                               "Gloomhaven"
                               (str "GH-" num)

                               "Frosthaven"
                               num

                               nil)]
                      (assoc card :id id))))
             (filter #(contains? available-ids (:id %)))
             (map (fn [m]
                    (merge m
                           {:purchasable (contains? purchasable
                                                    (:id m))
                            :craftable (contains? craftable
                                                  (:id m))})))
             (remove #(str/ends-with? (:image %) "-back.png"))
             (into [] (distinct-by :xws))
             (sort-by :points >))]
    (case s
      "craftable"
      (->> available-items
           (filter :craftable))

      ("purchasable" "purchaseable")
      (->> available-items
           (filter :purchasable))

      ;; else
      (let [reg (fuzzy-regex s)
            matches (->> available-items
                         (filter #(str/includes? (:xws %) s)))]
        matches))))

(defn help-text []
  (let []
    (str
     "Query commands:

*initiative*: 20i -20i 20-i 20-40i
*level*: 20l -20l 20-l 20-40l
*character*: " (clojure.string/join ", " character?) "
*expansion*: " (clojure.string/join ", " (keys expansion?)) "
*text*: \"shield spikes\"

or stats:

`/gh stats`

or for items:

`/gh item [query]`
`/gh item craftable`
`/gh item purchasable`

or for buildings:
`/gh buildings` for current buildings
`/gh buildings next` for next level buildings.

"))
  )

(defn gloom-command [request]
  (let [text (get-in request [:form-params "text"])]
    (cond

      (or (= text "help")
          (empty? text))
      {:body (json/write-str
              {"response_type" "ephemeral"
               "blocks" [{"type" "section"
                          "text" {"type" "mrkdwn"
                                  "text" (help-text)}}]}) 
       :headers {"Content-type" "application/json"}
       :status 200}

      (str/starts-with? text "building")
      (do
        (future
          (try
            (let [kind (case text
                         ("building" "buildings" "building current" "buildings current")
                         :current

                         :next)
                  response-url (get-in request [:form-params "response_url"])]
              (client/post response-url
                           {:body (json/write-str (building-response kind))
                            :headers {"Content-type" "application/json"}}))
            (catch Exception e
              (prn e))))
        {:body (json/write-str
                {"response_type" "in_channel"})
         :headers {"Content-type" "application/json"}
         :status 200})

      (or (str/starts-with? text "items")
          (str/starts-with? text "item"))
      (do
        (future
          (try
            (let [query-str (or (second (str/split text #" " 2))
                                "")
                  _ (prn "items" query-str)
                  items (find-items query-str)
                  response-url (get-in request [:form-params "response_url"])]
              (client/post response-url
                           {:body (if (seq items)
                                    (json/write-str
                                     (item-response items 0))
                                    (markdown-response "No cards found."))
                            :headers {"Content-type" "application/json"}}))
            (catch Exception e
              (prn e))))
        {:body (json/write-str
                {"response_type" "in_channel"})
         :headers {"Content-type" "application/json"}
         :status 200})

      (#{"stat" "stats"} text)
      (do
        (future
          (let [response-url (get-in request [:form-params "response_url"])]
            (client/post response-url
                         {:body (json/write-str
                                 (stats-response))
                          :headers {"Content-type" "application/json"}})))
        {:body (json/write-str
                {"response_type" "in_channel"})
         :headers {"Content-type" "application/json"}
         :status 200})

      :else
      (let [cards (query-cards cards text)]
        {:body (if (seq cards)
                 (json/write-str
                  (card-response cards 0)
                  #_{"response_type" "in_channel"
                     "blocks" (cards-block cards)})
                 "No cards found.")
         :headers {"Content-type" "application/json"}
         :status 200})
      )))

(defn frostrules-command [request]
  (let [text (get-in request [:form-params "text"])
        response-url (get-in request [:form-params "response_url"])]
    (println "rules" ": " text)
    (send-rules-response
     {:response-url response-url
      :text text})
    {:body (json/write-str
            {"response_type" "in_channel"}) 
     :headers {"Content-type" "application/json"}
     :status 200}))

(comment
  (require '[membrane.skia :as skia]
           '[membrane.ui :as ui])

  (defn card-viewer [cards]
    (ui/table-layout
     (into []
           (comp
            (map (fn [{:keys [image]}]
                   (ui/image
                    (.getAbsolutePath
                     (io/file worldhaven-root
                              "images"
                              image))
                    [400])))
            (partition-all 5))
           (->> cards
                ;; (filter #(= xws (:xws %)))
                (remove #(#{"A" "-" "B"} (:level %)))
                (map (fn [{:keys [level] :as card}]
                       (if (= level "X")
                         (assoc card :nlevel "1")
                         (assoc card :nlevel level))))
                (sort-by (juxt
                          #(not= "1" (:level %))
                          #(not= "X" (:level %))
                          #(Long/parseLong (:nlevel %))
                          #(Long/parseLong (:initiative %))))))
     4 4))

  (skia/run #(card-viewer "voidwarden"))

  (require '[com.phronemophobic.membrane.schematic3 :as s3])
  (alter-var-root #'s3/eval-ns (constantly *ns*))
  (s3/show!)

  (doseq [xws ["voidwarden"
               "redguard"
               "quartermaster"
               "summoner"]]
    (skia/save-image (str xws "-" "cards.jpeg")
                     (card-viewer xws)))

  ,)
