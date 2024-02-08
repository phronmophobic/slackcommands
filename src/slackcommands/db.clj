(ns slackcommands.db
  (:require [clojure.java.io :as io]
            [datalevin.core :as d]))


(def schema {})

(defonce conn
  (delay
    (d/get-conn "main.db" schema)))

(do
  (defn transact! [& args]
    (apply d/transact! @conn args))
  ;; add docs
  (let [mta (meta #'d/transact!)]
    (alter-meta! #'transact!
                 assoc
                 :doc (:doc mta)
                 :arglists
                 (->> (:arglists mta)
                      (map (fn [lst]
                             (vec (drop 1 lst))))))))


(do
  (defn q [query & args]
    (apply d/q query (d/db @conn) args))
  ;; add docs
  (let [mta (meta #'d/q)]
    (alter-meta! #'q
                 assoc
                 :doc (:doc mta)
                 :arglists (:arglists mta))))


