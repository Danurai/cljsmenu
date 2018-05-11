(ns cljsmenu.core
  (:require [reagent.core :as r]))
  
(enable-console-print!)
  
(def menu (r/atom {:hidden true :left "0px" :top "0px" :title "Menu" :buttons [{:title "one" :data-action "dosomething1"} {:title "two" :data-action "dosomething12"}]}))

(def deck (r/atom {}))

(def rawdeck
  {:010006 1 :010123 4 :010124 1 :010125 2 :010126 1 :010128 1 :010127 1 :010137 1 :010129 2 :010133 1 :010134 2 :010136 1 :010131 1 :010138 1 :010135 1 :010132 1 :010130 1 :010171 1 :010170 1 :010174 1 :010172 1 :010169 1 :010173 1 :010143 1 :010145 1 :010144 1 :010139 1 :010140 1 :010142 1 "010141":1}
  )

(defn toggle-popupmenu [e] (swap! menu assoc :hidden (->> @menu :hidden ((complement true?))) 
                                       :left (str (-> e .-pageX (- 20)) "px")
                                       :top (str (-> e .-pageY (- 20)) "px")))
(defn- close-popupmenu []
  (swap! menu assoc :hidden true))
(defn- do-action [act]
  (println act))
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

(defn cards []
  [:div
    [:div.card-wrap [:img.card {:src "/img/cardback.png" :alt "deck" :on-click #(toggle-popupmenu %)}]]
    [:div.card-wrap [:img.card {:alt "warlord"}]]
    [:div (str rawdeck)]
    ])

(defn renderpage []
  [:div
    [cards]
    [ctxmenu]
  ])

(r/render [renderpage] (.getElementById js/document "app"))


