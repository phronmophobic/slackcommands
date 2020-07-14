(ns slackcommands.core
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [slingshot.slingshot :refer [throw+ try+]])
  (:gen-class))

(defn get-secrets []
  (json/read (clojure.java.io/reader
              (clojure.java.io/resource "secrets.json"))))

;; https://slack.com/help/articles/360001623607-Create-commands-for-Slack-apps

(defn get-access-token []
  (let [result (client/get "https://us.battle.net/oauth/token"
                           {:query-params
                            (merge
                             {"grant_type" "client_credentials"
                              "client_id" client-id
                              "client_secret" secret}
                             (get-secrets))})
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


(defn search-cards
  ([txt]
   (search-cards (get-token) txt 1))
  ([token txt retry-count]
   (try+
    (let [result (client/get "https://us.api.blizzard.com/hearthstone/cards"
                             {:headers {"Authorization" (str "Bearer " token)}
                              :query-params
                              {"locale" "en_US"
                               "textFilter" txt}})
          body (:body result)
          obj (json/read-json body)]
      obj)
    (catch [:status 401] e
      (println "unauthorized")
      (when (pos? retry-count)
        (refetch-token)
        (search-cards (get-token) txt (dec retry-count)))))))




(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
