(ns slackcommands.perplexity
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clj-http.client :as client]
            [slingshot.slingshot :refer [try+]]
            [slackcommands.util :as util]))


(def api-key (:perplexity/api-key
              (edn/read-string (slurp "secrets.edn"))))


;; curl --request POST \
;;      --url https://api.perplexity.ai/chat/completions \
;;      --header 'accept: application/json' \
;;      --header 'content-type: application/json' \
;;      --data '
;; {
;;   "model": "llama-3.1-sonar-small-128k-online",
;;   "messages": [
;;     {
;;       "role": "system",
;;       "content": "Be precise and concise."
;;     },
;;     {
;;       "role": "user",
;;       "content": "How many stars are there in our galaxy?"
;;     }
;;   ]
;; }
;; '


;; models
;; llama-3.1-sonar-huge-128k-online


(defn chat-completions [{:keys [model messages]}]
  (let [model (or model
                  "sonar")
        response (client/post "https://api.perplexity.ai/chat/completions"
                              {:accept :json
                               :form-params 
                               {:model model
                                :messages messages}
                               :content-type :json
                               :headers {"Authorization" (str "Bearer " api-key)}
                               :as :json})]
    (:body response)))

(comment

  (def response
    (chat-completions 
     {:messages [{:role "system"
                  :content "Be precise and concise."}
                 {:role "user"
                  :content "aslkjfsdlfkj"}]}))


,)
