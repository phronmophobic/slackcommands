(ns slackcommands.hs
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [slingshot.slingshot :refer [throw+ try+]]
            [slackcommands.util :as util]
            [clojure.core.memoize :as memo]
            [clojure.edn :as edn])
  (:gen-class))

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

(def battle-net-credentials
  (:battle-net
   (edn/read-string (slurp "secrets.edn"))))
;; https://slack.com/help/articles/360001623607-Create-commands-for-Slack-apps
(defn get-access-token []
  (let [result (client/post "https://us.battle.net/oauth/token"
                            {:query-params
                             (merge
                              {"grant_type" "client_credentials"}
                              battle-net-credentials)})
        body (:body result)
        obj (json/read-json body)
        token (:access_token obj)]
    token))


(defonce token-atm (atom nil))
(defn get-token []
  (if-let [token @token-atm]
    @token
    (let [token (swap! token-atm
                       (fn [token]
                         (if token
                           token
                           (future (get-access-token)))))]
      @token)))
(defn refetch-token []
  (reset! token-atm nil)
  (get-token))


(defn -get-meta-data
  ([]
   (-get-meta-data (get-token) 1))
  ([token retry-count]
   
   (try+
     (let [result (client/get "https://us.api.blizzard.com/hearthstone/metadata"
                              {:headers {"Authorization" (str "Bearer " (get-token))}
                               :query-params
                               {"locale" "en_US"}})
           body (:body result)
           obj (json/read-json body)]
       obj)
     (catch [:status 401] e
       (println "unauthorized")
       (when (pos? retry-count)
         (refetch-token)
         (-get-meta-data (get-token) (dec retry-count)))))))

(defn -search-cards
  ([txt]
   (-search-cards (get-token) txt 1))
  ([token txt retry-count]
   (try+
     (let [result (client/get "https://us.api.blizzard.com/hearthstone/cards"
                              {:headers {"Authorization" (str "Bearer " token)}
                               :query-params
                               (merge
                                {"locale" "en_US"
                                 }
                                (if (string? txt)
                                  {"textFilter" txt}
                                  txt))})
           body (:body result)
           obj (json/read-json body)]
       obj)
     (catch [:status 401] e
       (println "unauthorized")
       (when (pos? retry-count)
         (refetch-token)
         (-search-cards (get-token) txt (dec retry-count)))))))

(defn -get-card
  ([slug]
   (-get-card (get-token) slug 1))
  ([token slug retry-count]
   (try+
     (let [result (client/get (str "https://us.api.blizzard.com/hearthstone/cards/" slug)
                              {:headers {"Authorization" (str "Bearer " token)}
                               :query-params
                               {"locale" "en_US"}})
           body (:body result)
           obj (json/read-json body)]
       obj)
     (catch [:status 401] e
       (println "unauthorized")
       (when (pos? retry-count)
         (refetch-token)
         (-get-card (get-token) slug (dec retry-count)))))))


(def one-day (* 1000 86400))
(def one-hour (* 1000 60 60))
(def get-card (memo/ttl -get-card :ttl/threshold one-day))

(declare search-cards
         read-deck-code
         get-meta-data)
(defn clear-caches []
  (doseq [f [get-card search-cards read-deck-code get-meta-data ]]
    (memo/memo-clear! f)))


(defn -read-deck-code
  ([deck-code]
   (-read-deck-code (get-token) deck-code 1))
  ([token deck-code retry-count]
   (try+
     (let [result (client/get (str "https://us.api.blizzard.com/hearthstone/deck")
                              {:headers {"Authorization" (str "Bearer " token)}
                               :query-params
                               {"locale" "en_US"
                                "code" deck-code}})
           body (:body result)
           obj (json/read-json body)]
       obj)
     (catch [:status 401] e
       (println "unauthorized")
       (when (pos? retry-count)
         (refetch-token)
         (-read-deck-code (get-token) deck-code (dec retry-count))))))
  )

(def search-cards (memo/ttl -search-cards :ttl/threshold one-hour))


(def read-deck-code (memo/ttl -read-deck-code :ttl/threshold one-hour))


(def get-meta-data (memo/ttl -get-meta-data :ttl/threshold one-hour))



(defn parse-tokens [s]
  (let [regex #"\s?(\"[^\"]+\"|[\S^\"]+)"]
    (loop [parts []
           s s]
      (let [[whole part] (re-find regex s)]
        (if part
          (recur (conj parts part)
                 (subs s (count whole)))
          parts)))))



(defn token->command [token]
  (let [metadata (get-meta-data)
        rarities (set (map :slug (:rarities metadata)))
        classes (set (map :slug (:classes metadata)))
        types (set (map :slug (:types metadata)))
        minion-types (set (map :slug (:minionTypes metadata)))
        spell-schools (set (map :slug (:spellSchools metadata)))
        keywords (set (map :slug (:keywords metadata)))
        all-sets (map :slug (:sets metadata))
        sets (set all-sets)
        latest-set (->> all-sets
                        (remove #{"core"})
                        first)
        set-groups (into {"rotating" (->> metadata
                                          :setGroups
                                          (filter #(= "dragon" (:slug %)))
                                          first
                                          :cardSets)}
                         (concat
                          (for [sgroup (:setGroups metadata)]
                            [(:slug sgroup)
                             (:cardSets sgroup)])
                          (for [sgroup (:setGroups metadata)]
                            [(str "yo" (:slug sgroup))
                             (:cardSets sgroup)])))
        ]
    
    (cond-let [x]

      (re-find #"\"([^\"]+)\"" token)
      {"textFilter" (second x)}

      (re-find #"([0-9]{1,2})m" token)
      {"manaCost" (Integer/parseInt (second x))}

      (re-find #"([0-9]{1,2})/([0-9]{1,2})" token)
      {"attack" (Integer/parseInt (second x))
       "health" (Integer/parseInt (nth x 2))}

      (re-find #"([0-9]{1,2})/" token)
      {"attack" (Integer/parseInt (second x))}

      (re-find #"/([0-9]{1,2})" token)
      {"health" (Integer/parseInt (second x))}

      (= token "lego")
      {"rarity" "legendary"}

      (= token "uncollectible")
      {"collectible" "0"}

      (= token "collectible")
      {"collectible" "1"}

      (re-find #"cardid:(.*)" token)
      {"id" (second x)}

      ({"dh" "demonhunter"
        "pally" "paladin"} token)
      {"class" x}

      (rarities token)
      {"rarity" x}

      (classes token)
      {"class" x}

      (types token)
      {"type" x}

      (minion-types token)
      {"minionType" x}

      (keywords token)
      {"keyword" x}
      
      (spell-schools token)
      {"spellSchool" x}

      (sets token)
      {"set" x}

      (= token "new")
      {"set" latest-set}

      (set-groups token)
      {"set" (clojure.string/join "," x)}

      :else
      {"textFilter" token})))




(defn help-text []
  (let [metadata (get-meta-data)
        rarities (map :slug (:rarities metadata))
        classes (map :slug (:classes metadata))
        types (map :slug (:types metadata))
        minion-types (map :slug (:minionTypes metadata))
        spell-schools (set (map :slug (:spellSchools metadata)))
        keywords (map :slug (:keywords metadata))
        sets (map :slug (:sets metadata))
        set-groups (map :slug (:setGroups metadata))]
    (str
     "Query commands:

*mana*: 4m
*attack/health*: 7/7
*attack/*: 7/
*/health*: /7
*rarity*: " (clojure.string/join ", " rarities) "
*classes*: " (clojure.string/join ", " classes) "
*types*: " (clojure.string/join ", " types) "
*minion types*: " (clojure.string/join ", " minion-types) "
*spell schools*: " (clojure.string/join ", " spell-schools) "
*keywords*: " (clojure.string/join ", " keywords) "
*sets*: " (clojure.string/join ", " sets) "
*set groups*: " (clojure.string/join ", " set-groups) "
*text*: \"give bananas\"

"))
  )

(defn merge-with-k
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter)."
  {:added "1.0"
   :static true}
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if (contains? m k)
			    (assoc m k (f k (get m k) v))
			    (assoc m k v))))
          merge2 (fn [m1 m2]
		   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))



(declare hard-coded)

(defn parse-query [s]
  (if-let [cmd (get hard-coded s)]
    cmd
    (let [tokens (parse-tokens s)
          command (reduce (fn [command token]
                            (when command
                              (when-let [subcommand (token->command token)]
                                ;; can't have more than one keyword :-/
                                (let [subcommand (if (and (contains? subcommand "keyword")
                                                          (contains? command "keyword"))
                                                   (-> subcommand
                                                       (dissoc "keyword")
                                                       (update "textFilter" (fn [s]
                                                                              (if s
                                                                                (str s "," (get subcommand "keyword"))
                                                                                (get subcommand "keyword")))))
                                                   subcommand)]
                                  (merge-with-k
                                   (fn [k s1 s2]
                                     (if (= k "textFilter")
                                       (str s1 " " s2)
                                       (str s1 "," s2)))
                                   command subcommand)))))
                          {}
                          tokens)]
      (when command
        (if (contains? command "set")
          command
          (merge command (token->command "standard")))))))

(def hard-coded
  {"infinite value" (parse-query "priest hero galakrond")})

(declare list-cards-response)
(defn list-cards-interact [payload {:keys [search]}]
  (let [;; [_ search] event
        url (get payload "response_url")
        ]
    (future
      (try
        (client/post url
                     {:body (json/write-str
                             (assoc (list-cards-response search (:cards (search-cards (parse-query search))))
                                    "replace_original" true))
                      :headers {"Content-type" "application/json"}})
        (catch Exception e
          (prn e))))
    {:body "ok"
     :headers {"Content-type" "application/json"}
     :status 200}))

(declare card-response)
(defn get-card-interact [payload {:keys [search index]}]
  (let [ ;; [_ search index] event
        url (get payload "response_url")]
    (future
      (try
        (client/post url
                     {:body (json/write-str
                             (assoc (card-response search
                                                   (:cards (search-cards (parse-query search)))
                                                   index)
                                    "replace_original" true))
                      :headers {"Content-type" "application/json"}})
        (catch Exception e
          (prn e))))
    {:body "ok"
     :headers {"Content-type" "application/json"}
     :status 200}))

(defn list-cards-response [search cards]
  (let [header {"type" "section"
                "text" {"type" "mrkdwn",
                        "text" search}
                "accessory" {"type" "button"
                             "text" {"type" "plain_text"
                                     "text" "delete"}
                             "value" (util/make-action `util/delete-message)}}
        main-blocks (for [[i card] (map-indexed vector cards)]
                      {"type" "section"
                       "text" {"type" "mrkdwn",
                               "text" (str
                                       "*" (:name card) "*: " (:manaCost card) "m "
                                       (when (:attack card)
                                         (str (:attack card) "/"
                                              (get card :health
                                                   (get card :durability))))
                                       " ")}
                       "accessory" {"type" "button"
                                    "text" {"type" "plain_text"
                                            "text" "view"}
                                    "value" (util/make-action
                                             `get-card-interact
                                             {:search search
                                              :index i})}
                       })]
    {"response_type" "in_channel"
     "blocks" (into [header]
                    main-blocks)}))

(defn card-response [search cards index]
  (let [card (nth cards index)
        card-set-id (get card :cardSetId)
        card-set-name (->> (get-meta-data)
                           :sets
                           (filter #(= card-set-id (get % :id)))
                           first
                           :name)
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
                               "text" (str (:name card)
                                           " "
                                           "(" card-set-name ")")},
                      "image_url" (:image card)
                      "alt_text" (str card-set-name ": " (:flavorText card))}
                     #_{"type" "context",
                        "elements"
                        (for [c  (take 10 cards)]
                          {"type" "image"
                           "alt_text" (:name c)
                           "image_url"  (:cropImage c)})
                        
                        }]
        parent-cards (when-let [pid (:parentId card)]
                       (search-cards {:id pid}))
        child-cards (when (seq (:childIds card))
                      (search-cards {:id (clojure.string/join ","
                                                              (:childIds card))}))
        related-cards (concat (:cards parent-cards)
                              (:cards child-cards))
        main-blocks (into main-blocks
                          (when (seq related-cards)
                            [{"type" "actions",
                              "elements"
                              (for [related-card related-cards]
                                (let []
                                  {"type" "button",
                                   "text"
                                   {"type" "plain_text", "text" (:name related-card)}
                                   "value" (util/make-action
                                            `get-card-interact
                                            {:search (str "cardid:" (:id related-card))
                                             :index 0})}))}]))]
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
                         "value" #_(make-action [:getcard search i])
                         (util/make-action
                          `get-card-interact {:search search
                                              :index i})})
                      
                      [{"type" "button",
                        "text"
                        {"type" "plain_text", "text" "..."}
                        "value" ;;(make-action [:list-cards search])
                        (util/make-action `list-cards
                                          {:search search})}]))
               }]))})
  )

(defn card-dust [rarity-id]
  (let [metadata (get-meta-data )
        rarities (:rarities metadata)
        rarity (some #(when (= (:id %) rarity-id)
                        %)
                     rarities)]
    (or (get-in rarity [:craftingCost 0])
        0)))

(defn deck-code-response [text]
  (let [parts (clojure.string/split text #" +")]
    (if (= (count parts) 2)
      (try+
        (let [deck-code (nth parts 1)
              response (-read-deck-code deck-code)
              cards (:cards response)
              card-strs (->> (group-by :slug cards)
                             (sort-by (fn [[slug cards]]
                                        (->> cards
                                             first
                                             :manaCost)))
                             (map (fn [[slug cards]]
                                    (let [card (first cards)]
                                      (str (if (>= (count cards) 2) "2x" "  ")
                                           " "
                                           (:manaCost card)
                                           " "
                                           (case (:rarityId card)
                                             5 "**"
                                             4 "*"
                                             ;; else
                                             nil
                                             )
                                           (:name card))))))]
          {"response_type" "in_channel"
           "blocks" [{"type" "section"
                      "text" {"type" "mrkdwn"
                              "text"
                              (str "```"
                                   "Class: " (:name (:class response))
                                   "\n"
                                   "Dust: " (apply + (map #(card-dust (:rarityId %)) cards))
                                   "\n\n"
                                   (clojure.string/join "\n" card-strs)
                                   "```")}}]})
        (catch [:status 400] e
          {"response_type" "ephemeral"
           "blocks" [{"type" "section"
                      "text" {"type" "mrkdwn"
                              "text" (str "Error parsing deck code.\n")}}]}))
      {"response_type" "ephemeral"
       "blocks" [{"type" "section"
                  "text" {"type" "mrkdwn"
                          "text" (str "Error parsing deck code.\n")}}]})))






(defn hs-command [request]
  (let [text (get-in request [:form-params "text"])]
    (cond
      (.startsWith text "deck")
      {:body (json/write-str (deck-code-response text))
       :headers {"Content-type" "application/json"}
       :status 200}

      (#{"clear-cache" "clearcache"} text)
      (do
        (clear-caches)
        {:body (json/write-str
                {"response_type" "ephemeral"
                 "blocks" [{"type" "section"
                            "text" {"type" "mrkdwn"
                                    "text" "cache cleared!"}}
                           ]})
         :headers {"Content-type" "application/json"}
         :status 200})
      
      (= text "help")
      {:body (json/write-str
              {"response_type" "ephemeral"
               "blocks" [{"type" "section"
                          "text" {"type" "mrkdwn"
                                  "text" (help-text)}}
                         ]}) 
       :headers {"Content-type" "application/json"}
       :status 200}

      

      :else
      (if-let [command (when (not= "help" text)
                         (parse-query text))]
        (let [cards (:cards (search-cards command))]
          (prn command)
          {:body (if (seq cards)
                   (json/write-str (card-response text cards 0))
                   "No cards found.")
           :headers {"Content-type" "application/json"}
           :status 200})
        {:body (json/write-str
                {"response_type" "ephemeral"
                 "blocks" [{"type" "section"
                            "text" {"type" "mrkdwn"
                                    "text" (str "Error parsing query.\n" (help-text))}}
                           ]}) 
         :headers {"Content-type" "application/json"}
         :status 200}))))



