(ns slackcommands.party
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn party-file [party-name]
  (when (or (not (re-matches #"[a-zA-Z0-9\-_.]+"
                             party-name))
            (> (count party-name)
               32))
    (throw (IllegalArgumentException. "Illegal Party!")))
  (let [party-dir (io/file "parties")]
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



(defn party-handler [request]
  (let [text (get-in request [:form-params "text"])
        parts (clojure.string/split text #"\s+" 2)]

    (case (count parts)
      ;; get
      1
      (let [party-name (str/trim text)
            party (if (= "" party-name)
                    ":party: :party_parrot: :penguinparty: :partytoad: :party-blob: :pandaparty: :meow-party: :this-is-fine-party:"
                    (try
                      (get-party party-name)
                      (catch IllegalArgumentException e
                        ":frogsiren:")))]
        
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
                                      "text" ":frogsiren: Party name must match [a-zA-Z0-9-_.]+"}}]})
           :headers {"Content-type" "application/json"}
           :status 200})))
    
    ))
