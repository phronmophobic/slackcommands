(ns slackcommands.gloom
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clj-http.client :as client]
            [clojure.core.memoize :as memo]
            [clojure.data.json :as json])
  #_(:import com.github.benmanes.caffeine.cache.Caffeine
           java.util.concurrent.TimeUnit))

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

(defn card-level [card]
  (case (:level card)
    ("-" "A" "B" "M" "P" "V" "X") 1
    ("1" "2" "3" "4" "5" "6" "7" "8" "9") (parse-long (:level card))))

(defn card-initiative [card]
  (parse-long (:initiative card)))

(defn image-url [card]
  (str "https://raw.githubusercontent.com/any2cards/worldhaven/master/images/"
       (:image card)))

(def ^:dynamic action-data nil)
(defn -get-action [s]
  (assert action-data)
  action-data)
(def one-day (* 1000 86400))
(def get-action (memo/ttl -get-action :ttl/threshold one-day))
(defn make-action [data]
  (binding [action-data data]
    (let [s (str "gh" (hash data))]
      (get-action s)
      s)))

(defn list-cards-response [cards]
  (let [header {"type" "section"
                "text" {"type" "mrkdwn",
                        "text" "all cards"}
                "accessory" {"type" "button"
                             "text" {"type" "plain_text"
                                     "text" "delete"}
                             "value" (make-action [:delete-message])}}
        main-blocks (for [[i card] (map-indexed vector cards)]
                      {"type" "section"
                       "text" {"type" "mrkdwn",
                               "text" (str

                                       "*" (:name card) "*: "
                                       "lvl " (:level card) " "
                                       "(" (:initiative card) ") - "
                                       (:character-xws card " ") " "
                                       " ")}
                       "accessory" {"type" "button"
                                    "text" {"type" "plain_text"
                                            "text" "view"}
                                    "value" (make-action [:getcard cards i])}
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
                                   "value" (make-action [:delete-message])}}

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
                         "value" (make-action [:getcard cards i])})
                      [{"type" "button",
                          "text"
                        {"type" "plain_text", "text" "..."}
                        "value" (make-action [:list-cards cards])}]))
               }]))})
  )

(defn gh-command-interact [request]
  (let [payload (json/read-str (get (:form-params request) "payload"))
        url (get payload "response_url")
        action (-> payload
                   (get "actions")
                   first
                   (get "value"))
        [action-type & action-args :as event] (get-action action)]

    (case action-type

      :delete-message
      (do
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
         :status 200})

      :list-cards
      (let [[_ cards] event]
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
             :status 200})

      :getcard
      (let [[_ cards index] event]
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
         :status 200}))))

;; LoadingCache<Key, Graph> graphs = Caffeine.newBuilder()
;;     .weakKeys()
;;     .weakValues()
;;     .build(key -> createExpensiveGraph(key));
#_(defn- make-cache [keyfn]
    (-> (Caffeine/newBuilder)
        (.expireAfterAccess 5, TimeUnit/MINUTES)
        (.softValues)
        (.build keyfn)))


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
  (let [cards-by-xws (group-by :xws cards)]
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

(defn character-filter [character-xws]
  (filter #(= character-xws
              (:character-xws %))))

(def special?
  {"mrgreen"
   {:character"redguard"}

   "mrgarbage"
   {:character "quartermaster"}
   
   "voidy"
   {:character "voidwarden"}
   
   })

(def character?
  (into #{}
        (map :character-xws)
        cards))

(defn token->command [token]
  (cond-let [x]

    (special? token)
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
                                     "Jaws Of The Lion"]))
        command (if (:character command)
                  command
                  (assoc command
                         :character
                         ["voidwarden"
                          "redguard"
                          "quartermaster"
                          "summoner"]))]
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



(defn query-cards [cards q]
  (let [command (->> (parse-query q)
                     
                     )
        xform (comp
               (remove #(#{"-" "A" "B" "M" "P" "V"}
                         (:level %)))
               (command->xform command))]
    (standard-sort
     (into []
           xform
           cards)))
  )


(defn help-text []
  (let []
    (str
     "Query commands:

*initiative*: 20i -20i 20-i 20-40i
*level*: 20l -20l 20-l 20-40l
*character*: " (clojure.string/join ", " character?) "
*expansion*: " (clojure.string/join ", " (keys expansion?)) "
*text*: \"shield spikes\"

"))
  )

(defn gloom-command [request]
  (let [text (get-in request [:form-params "text"])]
    (cond
      (= text "help")
      {:body (json/write-str
              {"response_type" "ephemeral"
               "blocks" [{"type" "section"
                          "text" {"type" "mrkdwn"
                                  "text" (help-text)}}]}) 
       :headers {"Content-type" "application/json"}
       :status 200}

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