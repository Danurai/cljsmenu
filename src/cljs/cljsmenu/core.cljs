(ns cljsmenu.core
  (:require 
    [reagent.core :as r]
    [goog.net.XhrIo :as xhr]
    [goog.events :as events]
    [cljs.core.async :as async :refer [>! chan close!]])
  (:require-macros
    [cljs.core.async.macros :refer [go alt!]]))
  
(enable-console-print!)

(defn GET [url]
  (let [ch (chan 1)]
    (xhr/send url
              (fn [event]
                (let [res (-> event .-target .getResponseText)]
                  (go (>! ch res)
                      (close! ch)))))
    ch))
    
(def menu (r/atom {:hidden true 
                 :left "0px" 
                 :top "0px"}))

(def deck (r/atom {}))

(def appstate (r/atom {:img nil :planets nil}))

(def cards (r/atom nil))

(defn- getcard [r]  
  (->> @cards 
      (filter #(= (:code %) r))
      first))
  
(defn build-deck [rawdeck]
  (let [all-cards (->> (map (fn [x] (repeat (x rawdeck) (getcard (name x)))) (keys rawdeck)) (reduce concat))]
    (reset! deck {:warlord (->> all-cards (filter #(= (:type_code %) "warlord_unit")) first)
                :resources (->> all-cards (filter #(= (:type_code %) "warlord_unit")) first :starting_resources)
                :decklist (shuffle (map-indexed #(assoc %2 :idx %1 :pos :deck) (filter #(not= (:type_code %) "warlord_unit") all-cards)))})
  ))
  
(defn drawcard []
  (swap! deck update-in [:decklist 
                      (->> @deck :decklist (take-while #(not= (:pos %) :deck)) count)] 
                      assoc :pos :hand))

(defn shuffledeck []
  )
                        
(defn toggle-popupmenu [e] (swap! menu assoc :hidden (->> @menu :hidden ((complement true?))) 
                                        :left (str (-> e .-pageX (- 20)) "px")
                                        :top (str (-> e .-pageY (- 20)) "px")))
(defn- close-popupmenu []
  (swap! menu assoc :hidden true))
(defn- do-action [act]
  (case act
    "draw" (drawcard)
    ""))

(defn hq-click [e]
  (swap! menu assoc :title "Draw"
                  :buttons [{:title "Draw" :data-action "draw"}])
  (toggle-popupmenu e))
    
(defn play-card [idx e]
  (let [card (->> @deck :decklist (filter #(= (:idx %) idx)) first)]
    (swap! menu assoc :title (:name card)
                    :buttons [{:title "P1" :data-action '(deploy-card (:idx card) :p1)}])
    (toggle-popupmenu e)))
    
    
    

(defn ctxmenu []
  [:div#popupmenu.popupmenu {:hidden (:hidden @menu) 
                          :style {:left (:left @menu) :top (:top @menu)}}
    [:button.close {:on-click #(close-popupmenu)} "\u00D7"]
    [:div.popupmenu-header (:title @menu)]
    [:div.btn-group.btn-group-sm.btn-group-vertical.d-flex
      (for [button (:buttons @menu)]
        ^{:key (:title button)}
          [:button.btn.btn-block.btn-light 
            {:data-action (:data-action button)
             :on-click (fn [] (do-action (:data-action button)) (close-popupmenu))}
            (:title button)])]])

(defn hq []
  [:div#hq[:div "HQ"]
    [:div
      [:div#deck.card-wrap
        [:img.card {:src "/img/cardback.png" :alt "deck" :on-click #(hq-click %)}]
        [:span.cardcount (->> @deck :decklist (filter #(= (:pos %) :deck)) count)]]
      [:div.card-wrap
        [:img.card {:src (-> @deck :warlord :img) :alt "warlord"}]
        [:span.resources (->> @deck :resources)]]]
    [:div#hand [:div "Hand"]
      (doall (for [r (->> @deck :decklist (filter #(= (:pos %) :hand)))]
        ^{:key (:idx r)}
          [:div.card-wrap 
            [:img.card {:src (:img r)
                        :alt (:name r)
                        :on-click #(play-card (:idx r) %)
                        ;:on-mouse-out #(swap! appstate assoc :img nil)
                        :on-mouse-over #(swap! appstate assoc :img (:img r))}]]))]
    ])

(defn renderpage []
  [:div.container-fluid {}
    [:div.row
      [:div.col-sm-9
        [hq]]
      [:div.col-sm-3
        [:div.cardimg [:img {:hidden (nil? (:img @appstate)) :src (:img @appstate)}]]]
      [ctxmenu]
    ;[:div (str @appstate)]
  ]])

(go 
  (reset! cards (:data (js->clj (.parse js/JSON (<! (GET "http://localhost:9009/api/data/cards"))) :keywordize-keys true)))
  (build-deck  {:010006 1 :010123 4 :010124 1 :010125 2 :010126 1 :010128 1 :010127 1 :010137 1 :010129 2 :010133 1 :010134 2 :010136 1 :010131 1 :010138 1 :010135 1 :010132 1 :010130 1 :010171 1 :010170 1 :010174 1 :010172 1 :010169 1 :010173 1 :010143 1 :010145 1 :010144 1 :010139 1 :010140 1 :010142 1 :010141 1})
  (dotimes [n 7] (drawcard))
  ;(events/listen )
  (r/render [renderpage] (.getElementById js/document "app")))