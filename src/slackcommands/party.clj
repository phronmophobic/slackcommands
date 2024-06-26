(ns slackcommands.party
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [slackcommands.slack :as slack]
            [clojure.java.io :as io])
  (:import java.util.regex.Pattern))

;;


(defn split-on-numbers
  "Split a string into a sequence of parts where numbers and non-numbers
  are separated."
  [s]
  (let [re (re-seq #"(?<num>\d+)|(?<notnum>\D+)" s)]
    (map 
     (fn [[_ num not-num]]
       (if num
         (parse-long num)
         not-num))
     re)))

(defn natural-compare
  "Compare two strings in a natural order."
  [s1 s2]
  (let [parts-a (split-on-numbers s1)
        parts-b (split-on-numbers s2)]
    (loop [pa parts-a, pb parts-b]
      (cond
        (and (empty? pa) (empty? pb)) 0
        (empty? pa) -1
        (empty? pb) 1
        :else
        (let [a (first pa)
              b (first pb)
              cmp (if (= (type a)
                         (type b))
                    (compare a b)
                    (compare (str a) (str b)))]
          (if (zero? cmp)
            (recur (rest pa) (rest pb))
            cmp))))))

(defn natural-sort
  "Sort a collection of strings in natural order."
  [coll]
  (sort natural-compare coll))

;; Example usage
(comment
  (def strings ["steamed-hams-9" "steamed-hams-10" "steamed-hams-2" "steamed-hams-1"])
  (def sorted-strings (natural-sort strings))

  (prn sorted-strings)
  ,)

;; 


(defn party-file [party-name]
  (when (or (not (re-matches #"[a-zA-Z0-9\-_.:]+"
                             party-name))
            (> (count party-name)
               32))
    (throw (IllegalArgumentException. "Illegal Party!")))
  (let [party-dir (io/file "parties")
        party-name (str/replace party-name #":" "_")]
    (.mkdir party-dir)

    (when (> (count (.listFiles party-dir))
             100)
      (throw (IllegalArgumentException. "Illegal Party!")))
    (io/file party-dir party-name)))

(defn set-party! [party-name party]
  (spit (party-file party-name) party))

(defn get-party [party-name]
  (let [pf (party-file party-name)]
    (when (.exists pf)
      (slurp pf))))

(defn list-parties []
  (sort
   (eduction
    (filter #(.isFile %))
    (map #(.getName %))

    (.listFiles (io/file "parties")))))

(defn party-handler [request]
  (let [text (get-in request [:form-params "text"])
        text (str/trim text)
        parts (clojure.string/split text #"\s+" 2)]

    (case (count parts)
      ;; get
      1
      (let [party-name (str/trim text)
            party (case party-name

                    ""
                    ":party: :party_parrot: :penguinparty: :partytoad: :party-blob: :pandaparty: :meow-party: :this-is-fine-party:"

                    "?"
                    (str "```\n"
                         (clojure.string/join ", " (list-parties))
                         "\n```")

                    ;; else
                    (cond
                      (str/includes? party-name "*")
                      (let [matches (->> (keys (slack/list-emoji))
                                         (map name)
                                         (filter #(re-matches 
                                                   (Pattern/compile
                                                    (str "^"
                                                         (str/replace party-name #"\*" ".*")
                                                         "$"))
                                                   %)))]
                        (when (seq matches)
                          (str/join
                           " "
                           (eduction
                            (map #(str ":" % ":"))
                            (natural-sort matches)))))

                      :else
                      (try
                        (if (re-matches #"^:[a-zA-Z0-9-_]+:$" party-name)
                          (clojure.string/join (repeat 20 party-name))
                          (get-party party-name))
                        (catch IllegalArgumentException e
                          ":frogsiren:"))))]
        
        {:body (json/write-str
                {"response_type" (if party
                                   "in_channel"
                                   "ephemeral")
                 "blocks" [{"type" "section"
                            "text" {"type" "mrkdwn"
                                    "text" (if party
                                             party
                                             ":shruggy: could not find party :sadpanda:")}}]})
         :headers {"Content-type" "application/json"}
         :status 200})


      ;; set party
      2
      (try
        (let [party-name (first parts)
              party (second parts)]
          (set-party! party-name party)
          {:body (json/write-str
                  {"response_type" "ephemeral"
                   "blocks" [{"type" "section"
                              "text" {"type" "mrkdwn"
                                      "text" party}}]})
           :headers {"Content-type" "application/json"}
           :status 200})
        (catch IllegalArgumentException e
          {:body (json/write-str
                  {"response_type" "ephemeral"
                   "blocks" [{"type" "section"
                              "text" {"type" "plain_text"
                                      "emoji" true
                                      "text" ":frogsiren: Party name must match [a-zA-Z0-9-_.:]+"}}]})
           :headers {"Content-type" "application/json"}
           :status 200})))
    
    ))
