(ns slackcommands.core
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [slingshot.slingshot :refer [throw+ try+]]
            [org.httpkit.server :as server]
            [ring.util.response :as response]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.params]
            [ clojure.core.memoize :as memo]
            [compojure.core :refer [defroutes routes GET POST DELETE ANY context]
             :as compojure]
            [compojure.route :as route]
)
  (:gen-class))





(defn get-secrets []
  (json/read
   (clojure.java.io/reader
    (or (clojure.java.io/resource "secrets.json")
        (clojure.java.io/file "resources/secrets.json")))))

;; https://slack.com/help/articles/360001623607-Create-commands-for-Slack-apps

(defn get-access-token []
  (let [result (client/get "https://us.battle.net/oauth/token"
                           {:query-params
                            (merge
                             {"grant_type" "client_credentials"}
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


(defn -search-cards
  ([txt]
   (-search-cards (get-token) txt 1))
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
        (-search-cards (get-token) txt (dec retry-count)))))))

(def ten-minutes (* 1000 60 10))
(def search-cards (memo/ttl -search-cards :ttl/threshold ten-minutes))


(defn card-response [search cards index]
  (let [card (nth cards index)
        main-blocks [{"type" "section",
                     "text"
                     {"type" "mrkdwn",
                      "text"
                      (str (inc index) " of " (count cards) " matches.")}}
                     {"type" "divider"}
                     {"type" "image",
                      "title" {"type" "plain_text",
                               "text" (:name card)},
                      "image_url" (:image card)
                      "alt_text" (:flavorText card)}
                     #_{"type" "section",
                     "text"
                     {"type" "mrkdwn",
                      "text"
                      (get card :name)},
                     "accessory"
                     {"type" "image",
                      "image_url" (get card :cropImage)
                      "alt_text" (get card :flavorText)}}]]
    {"response_type" "in_channel"
     "blocks"
     (if (< (inc index) (count cards))
       (into main-blocks
             [{"type" "divider"}
              {"type" "actions",
               "elements"
               [{"type" "button",
                 "text"
                 {"type" "plain_text", "text" "Next Result"}
                 "value" (clojure.string/join "::" ["getcard" search (inc index)]) }]}])
       main-blocks)})
  )




(defn hs-command [request]
  (prn request)
  (let [text (get-in request [:form-params "text"])
        cards (:cards (search-cards text))]
    
    {:body (if (seq cards)
             (json/write-str (card-response text cards 0))
             "No cards found.")
     :headers {"Content-type" "application/json"}
     :status 200}))


(defn hs-command-interact [request]
  (let [payload (json/read-str (get (:form-params request) "payload"))
        url (get payload "response_url")
        action (-> payload
                   (get "actions")
                   first
                   (get "value"))
        [_ search index] (clojure.string/split action #"::")
        ]
    (prn action search index)
    
    (future
      (try
        (client/post url
                     {:body (json/write-str
                             (assoc (card-response search
                                                   (:cards (search-cards search))
                                                   (Long/parseLong index))
                                    "replace_original" true))
                      :headers {"Content-type" "application/json"}})
        (catch Exception e
          (prn e))))
    {:body "ok"
     :headers {"Content-type" "application/json"}
     :status 200}))

(def my-routes
  (routes

   (ANY "/getcard" [] hs-command)
   ;; (GET "/index.html" [] (response/resource-response "index.html" {:root "public"}))
   ;; (GET "/" [] (response/resource-response "index.html" {:root "public"}))
   ;; (GET "/" [] "You need to specify a room. Try http://wavelength.smith.rocks/roomName")
   ;; (GET "/:room{[a-zA-Z0-9.\\-]+}" [] (response/resource-response "index.html" {:root "public"}))
   ;; ;; (GET "/js/" [] (response/resource-response "js/compiled/berry.js" {:root "public"}))

   (ANY "/.*" [] hs-command)
   (ANY "/interact/getcard" [] hs-command-interact)
   ;; (GET "/bar" [] "Hello Bar")
   (route/not-found "Not Found"))
  )

(defn my-app [request]
  (let [handler (-> my-routes
                    (ring.middleware.params/wrap-params))]
    (handler request)))


(comment
  (def server (server/run-server #'my-app {:port 488}))
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def server (server/run-server #'my-app {:port 477}))
  )



#_(json/read

(clojure.java.io/reader (clojure.java.io/file "temp.json"))

 )

