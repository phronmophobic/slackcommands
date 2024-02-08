(ns slackcommands.db
  (:require [clojure.java.io :as io]
            [datalevin.core :as d]))


(def schema {})

(defonce conn
  (delay
    (d/get-conn "main.db" schema)))


(defn transact! 
  ([]
   (d/transact! )))
