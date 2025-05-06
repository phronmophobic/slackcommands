(ns slackcommands.core
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [slingshot.slingshot :refer [throw+ try+]]
            [org.httpkit.server :as server]
            [ring.util.response :as response]
            [slackcommands.util :as util]
            slackcommands.slack.events
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.params]
            [clojure.core.memoize :as memo]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [compojure.core :refer [defroutes routes GET POST DELETE ANY context]
             :as compojure]
            [compojure.route :as route]
            [slackcommands.gloom :as gloom]
            [slackcommands.hs :as hs]
            [slackcommands.party :as party]
            [slackcommands.ai :as ai])
  (:gen-class))

(defn slack-interact [request]
  (let [payload (json/read-str (get (:form-params request) "payload"))
        action (-> payload
                   (get "actions")
                   first
                   (get "value"))
        action-id (-> payload
                      (get "actions")
                      first
                      (get "action_id"))
        action-str (if (str/starts-with? action "action-")
                     action
                     action-id)
        {:keys [var-sym data]} (util/get-action action-str)]
    ((requiring-resolve var-sym) payload data)))


(def my-routes
  (routes

   (ANY "/gloomcard" [] gloom/gloom-command)

   (ANY "/getcard" [] hs/hs-command)
   ;; (GET "/index.html" [] (response/resource-response "index.html" {:root "public"}))
   ;; (GET "/" [] (response/resource-response "index.html" {:root "public"}))
   ;; (GET "/" [] "You need to specify a room. Try http://wavelength.smith.rocks/roomName")
   ;; (GET "/:room{[a-zA-Z0-9.\\-]+}" [] (response/resource-response "index.html" {:root "public"}))
   ;; ;; (GET "/js/" [] (response/resource-response "js/compiled/berry.js" {:root "public"}))

   (ANY "/this-is-fine" []
     {:body (json/write-str
             {"response_type" "in_channel"
              "blocks" [{"type" "section"
                         "text" {"type" "plain_text"
                                 "emoji" true
                                 "text" ":this-is-fine-party: :this-is-fine-fire: :thisisfine: :this-is-fine-but-with-ai: :this-is-fine-but-with-wind: :this-is-fine-but-with-flooding: :this-is-fine-but-with-lightning: :this-is-fine-but-with-earthquakes: :this-is-fine-but-with-bankruptcy: :this-is-fine-but-with-al: :meow-this-is-fine:"}}]})
      :headers {"Content-type" "application/json"}
      :status 200})


   (ANY "/party" []
     party/party-handler
     )

   (ANY "/parrot" []
     {:body (json/write-str
             {"response_type" "in_channel"
              "blocks" [{"type" "section"
                         "text" {"type" "plain_text"
                                 "emoji" true
                                 "text" ":party_parrot: :clj-parrot: :shipitparrot: :pirateparrot: :parrot: :parrot_mustache: :fixparrot: :sad_parrot: :fast-parrot: :conga_parrot: :mask-parrot: :portalparrot: :coffee_parrot: :deal_with_it_parrot:"}}]})
      :headers {"Content-type" "application/json"}
      :status 200})

   (ANY "/terminator-image" []
     ;; ai/stable-image-command
     ai/image-command)

   (ANY "/terminator-stable-image" []
     ai/stable-image-command)

   (ANY "/midjourney" []
     ai/midjourney-image-command)

   (ANY "/flux" []
     ai/flux-image-command)
   
   (ANY "/ideogram" []
     ai/ideogram-image-command)

   #_(GET "/aimages/ss.jpg"
         []
       (do
         (when-let [f (requiring-resolve 'com.phronemophobic.discord.viewer/update-debug)]
           (f))
         (response/file-response "aimages/ss.jpg")))


   (route/files "/aimages/"
                {:root (.getAbsolutePath slackcommands.util/aimage-dir)})


   (route/files "/.well-known/"
                {:root (-> (io/file "certbot" ".well-known")
                           (.getCanonicalPath))})


   #_(ANY "/oauth/redirect" []
       slackcommands.slack/oauth-redirect)

   (ANY "/slack/events" []
     slackcommands.slack.events/events-api)


   (ANY "/terminator-chat" []
     ai/chat-command)

   (ANY "/naighty-chat" req
     (ai/chat-command (assoc req :ai-type :naighty)))

   (ANY "/frostrules" []
     gloom/frostrules-command)

   (ANY "/seairch" []
     ai/seairch-command)

   #_(GET "/debug" []
       {:body (json/write-str (card-response "Onyx Magescribe"
                                             (:cards (search-cards "Onyx Magescribe" ))
                                             0

                                             ))
        :status 200})
   (ANY "/interact/getcard" [] slack-interact)
   ;; (GET "/bar" [] "Hello Bar")
   (route/not-found "Not Found"))
  )

(defn my-app [request]
  (let [handler (-> my-routes
                    (ring.middleware.params/wrap-params))]
    (handler request)))


(comment
  (def server (server/run-server #'my-app {:port 488}))
  (def server2 (server/run-server #'my-app {:port 3000}))

  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  ;; initiation protocol
  ;; 0. did you start with `xvfb-run -a` and `authbind --deep` :server alias?
  ;; 1. start server
  (def server2 (server/run-server #'my-app {:port 3000}))

  (comment
    ;; 2. run. did you start with xvfb-run?
    (com.phronemophobic.discord.viewer/start-browser-server)
    ;; 3. run
    (com.phronemophobic.discord.discord/run-discord-bot!)
    ,)
  
  
  )

(comment
  (require 'clojure.repl.deps)
  ,)





