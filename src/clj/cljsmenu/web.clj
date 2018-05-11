(ns cljsmenu.web
   (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [hiccup.page :as h]
            [compojure.core :refer [defroutes GET POST ANY context]]
            [compojure.route :refer [not-found resources]]
            [ring.util.response :refer [response resource-response content-type redirect]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.session :refer [wrap-session]]))

(defroutes app-routes
  (GET "/" req
    (h/html5 
      [:head
      ;; Meta Tags
        [:meta {:charset "UTF-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]]
        ;; jquery
        [:script {:src "https://code.jquery.com/jquery-3.3.1.min.js" :integrity "sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8=" :crossorigin "anonymous"}]
      ;; Bootstrap  
        [:link   {:rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" :integrity "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" :crossorigin "anonymous"}]
        [:script {:src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js" :integrity "sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl" :crossorigin "anonymous"}]
        (h/include-css "css/style.css")
      [:body
        [:div#app]
        (h/include-js "js/compiled/cljsapp.js")
        ]))
  (context "/api/data" []
    (GET "/cards" [] (content-type (response (slurp (io/resource "data/wh40k_cards.min.json"))) "application/json")))
  (resources "/"))
   
(def app 
  (-> app-routes
    (wrap-keyword-params)
    (wrap-params)
    (wrap-session)))
   