(ns cljsmenu.core
  (:require 
    [reagent.core :as r]
    [goog.net.XhrIo :as xhr]
    [goog.events :as events]
    [cljs.core.async :as async :refer [>! chan close!]])
  (:require-macros
    [cljs.core.async.macros :refer [go alt!]]))
  
(enable-console-print!)
; GET data from server
(defn GET [url]
  (let [ch (chan 1)]
    (xhr/send url
      (fn [event]
        (let [res (-> event .-target .getResponseText)]
          (go (>! ch res)
              (close! ch)))))
    ch))

; ATOMs - could be condensed to one appstate atom
(def menu     (r/atom {:hidden true :left "0px" :top "0px"}))
(def deck     (r/atom {}))
(def appstate  (r/atom {:img nil :planets nil}))
(def cards    (r/atom nil))
(def planets  (r/atom nil))
(def log      (r/atom ()))

(defn- dev? [] false)
(defn- devimg [src]
  (if (dev?) nil src))
  
; Initalisation and deck fns
(defn- getcard [code]  
  (->> @cards 
      (filter #(= (:code %) code))
      first))

(defn logaction [msg]
  (swap! log conj ^{:key (gensym)}[:p.logitem msg]))
      
(defn build-planets []
  (reset! planets 
    (take 7 
      (map-indexed 
        (fn [idx p] 
          (assoc p :id idx :revealed (< idx 5))) 
      (->> @cards (filter #(= (:type_code %) "planet")) shuffle)))))

(defn shuffledeck []
  (swap! deck assoc :decklist (-> @deck :decklist shuffle))
  (logaction "Shuffle Deck."))      
  
(defn card-draw []
  (let [drawid (->> @deck :decklist (filter #(= (:pos %) :deck)) first :idx)]
    (swap! deck assoc :decklist (map #(if (= (:idx %) drawid) (assoc % :pos :hand) %) (:decklist @deck)))
    (logaction "Draw Card.")))

    
(defn build-deck [rawdeck]
  (let [all-cards (->> (map (fn [x] (repeat (x rawdeck) (getcard (name x)))) (keys rawdeck)) (reduce concat))]
    (reset! deck {:warlord (->> all-cards (filter #(= (:type_code %) "warlord_unit")) first)
                :resources (->> all-cards (filter #(= (:type_code %) "warlord_unit")) first :starting_resources)
                :hp (->> all-cards (filter #(= (:type_code %) "warlord_unit")) first :hp)
                :bloodied false
                :decklist (map-indexed #(assoc %2 :idx %1 :pos :deck) (filter #(not= (:type_code %) "warlord_unit") all-cards))})
    (shuffledeck)
    (dotimes [n (-> @deck :warlord :starting_hand)] (card-draw))))

(defn do-setup  []
  (go 
    (reset! cards (:data (js->clj (.parse js/JSON (<! (GET "http://localhost:9009/api/data/cards"))) :keywordize-keys true)))
    (build-planets)
    (build-deck  {:010006 1 :010123 4 :010124 1 :010125 2 :010126 1 :010128 1 :010127 1 :010137 1 :010129 2 :010133 1 :010134 2 :010136 1 :010131 1 :010138 1 :010135 1 :010132 1 :010130 1 :010171 1 :010170 1 :010174 1 :010172 1 :010169 1 :010173 1 :010143 1 :010145 1 :010144 1 :010139 1 :010140 1 :010142 1 :010141 1})
    (reset! log ())
))

(defn hq-phase []
  ; assign next First Planet - in code
  ; Reveal next planet - in code
  ; Draw 2 cards
  (dotimes (n 2) (card-draw))
  ; Take 4 Resources
  (swap! deck update :resources + 4)
  ; Ready all exhausted cards
  (swap! deck assoc :decklist (map #(dissoc % :exhausted) (-> @deck :decklist)))
  ; Pass Initiative
  )

(defn card-discard [idx]
  (swap! deck assoc :decklist (map #(if (= (:idx %) idx) (assoc % :pos :discard) %)
                              (-> @deck :decklist))))
                              
(defn card-deploy [card-id planet-id]
  (let [crd (->> @deck :decklist (filter #(= (:idx %) card-id)) first)
       plnt (->> @planets (filter #(= (:id %) planet-id)) first)]
    (swap! deck update :resources - (:cost crd 0))
    (swap! deck assoc :decklist (map #(if (= (:idx %) card-id) (assoc % :pos planet-id) % ) (:decklist @deck)))
    (logaction [:span [:a {:href "#" :on-mouse-over #(swap! appstate assoc :img (:img crd))} (:name crd)] (str " deployed to " (if (some? plnt) (:name plnt) "HQ"))])))

(defn card-play [card-id]
  (let [crd (->> @deck :decklist (filter #(= (:idx %) card-id)) first)]
    (swap! deck update :resources - (:cost crd 0))
    (card-discard card-id)
    (logaction [:span "Played " [:a {:href "#" :on-mouse-over #(swap! appstate assoc :img (:img crd))} (:name crd)]])))
    
(defn card-toggle-exhaust [card-id] 
  (swap! deck assoc :decklist (map #(if (= (:idx %) card-id) (assoc % :exhausted (-> % :exhausted true? not)) %) (@deck :decklist) )))
  
(defn toggle-popupmenu [e]
  (swap! menu assoc :hidden (->> @menu :hidden ((complement true?)))
                  :left (str (-> e .-pageX (- 20)) "px")
                  :top (str (-> e .-pageY (- 20)) "px")))
(defn- close-popupmenu [] (swap! menu assoc :hidden true))

; Controller Functionality

(defn deck-click [e]
  (swap! menu assoc :buttons [{:title "Shuffle" :data-action shuffledeck}])
  (toggle-popupmenu e))

(defn discard-click [e]
  (prn e))
  
(defn hand-click [idx e]
  (let [card (->> @deck :decklist (filter #(= (:idx %) idx)) first)]
    (when (<= (:cost card 0) (:resources @deck))
      (swap! menu assoc :buttons
        (concat 
          (case (:type_code card) 
            "army_unit"  (remove nil? (map #(if (:revealed %) {:title (:name %) :data-action (fn [] (card-deploy (:idx card) (:id %)))}) @planets))
            "attachment" (remove nil? (map #(if (:revealed %) {:title (:name %) :data-action (fn [] (card-deploy (:idx card) (:id %)))}) @planets))
            "event"     [{:title "Play" :data-action #(card-play (:idx card))}]
            "support"   [{:title "Deploy to HQ" :data-action #(card-deploy (:idx card) -1)}]
            nil)
          [{:title "divider"}
           {:title "Discard" :data-action #(card-discard (:idx card))}]))
      (toggle-popupmenu e))))

(defn card-click [idx e]
  (swap! menu assoc :buttons [{:title "Discard" :data-action #(card-discard idx)}
                           {:title "Exhaust\\Ready" :data-action #(card-toggle-exhaust idx)}])
  (toggle-popupmenu e))

  
 
; Page Layout    

(defn ctxmenu []
  [:div#popupmenu.popupmenu {:hidden (:hidden @menu) 
                          :style {:left (:left @menu) :top (:top @menu)}}
    ;[:button.close {:on-click #(close-popupmenu)} "\u00D7"]
    ;[:div.popupmenu-header (:title @menu)]
    [:div.btn-group.btn-group-sm.btn-group-vertical.d-flex
      (for [button (:buttons @menu)]
        (if (= (:title button) "divider")
          ^{:key (:title button)}[:div.dropdown-divider]
          ^{:key (:title button)}
            [:button.btn.btn-block.btn-light 
              {:on-click #((:data-action button)(close-popupmenu))}
              (:title button)]))]])

(defn cardimgwrap [crd]
    ^{:key (:idx crd)}[:div.card-wrap 
      [:img.card {:class (if (true? (:exhausted crd)) "card-tapped")
                :src (devimg (:img crd)) 
                :alt (:name crd)
                :on-click #(card-click (:idx crd) %)
                :on-mouse-over #(swap! appstate assoc :img (:img crd))}]])
            
(defn hq []
  [:div#hq
    [:div "HQ"]
    [:div
      [:div#discard.card-wrap
        [:img.card {:src (devimg nil) :alt "discard" :on-click discard-click}]
        [:span.cardcount (->> @deck :decklist (filter #(= (:pos %) :discard)) count)]]
      [:div#deck.card-wrap
        [:img.card {:src (devimg "/img/cardback.png") :alt "deck" :on-click #(deck-click %)}]
        [:span.cardcount (->> @deck :decklist (filter #(= (:pos %) :deck)) count)]]
      [:div.card-wrap
        [:img.card {:src (devimg (-> @deck :warlord :img)) :alt (-> @deck :warlord :name)}]] ;(-> @deck :warlord :img)}]
      [:div.card-wrap.resources 
        [:div 
          [:span.mx-2 [:i.fas.fa-cog.fa-xs.mr-2] (:resources @deck)]
          [:span.btn-res {:on-click #(swap! deck update :resources dec)} "-"]
          [:span.btn-res {:on-click #(swap! deck update :resources inc)} "+"]]
        [:div 
          [:span.mx-2 [:i.fas.fa-heart.fa-xs.mr-2] (:hp @deck)]
          [:span.btn-res {:on-click #(swap! deck update :hp dec)} "-"]
          [:span.btn-res {:on-click #(swap! deck update :hp inc)} "+"]]
        [:div.m-1
          [:button.btn.btn-block.btn-outline-light.btn-sm
            {:on-click #(card-draw)} "Draw"]]
        [:div.m-1
          [:button.btn.btn-block.btn-outline-light.btn-sm
            {:on-click #(hq-phase)} "HQ Phase"]]]
      [:div.card-wrap
        (doall 
          (for [crd (->> @deck :decklist (filter #(= (:pos %) -1)))]
            (cardimgwrap crd)))]]
    [:div (str "Hand (" (->> @deck :decklist (filter #(= (:pos %) :hand)) count) ")")]
    [:div
      (doall (for [r (->> @deck :decklist (filter #(= (:pos %) :hand)))]
        ^{:key (:idx r)}
          [:div.card-wrap {:class (if (<= (:cost r 0) (-> @deck :resources)) "can-play" "no-play") }
            [:img.card {:src (devimg (:img r))
                        :alt (:name r)
                        :on-click #(hand-click (:idx r) %)
                        ;:on-mouse-out #(swap! appstate assoc :img nil)
                        :on-mouse-over #(swap! appstate assoc :img (:img r))}]]))]  ;(:img r)
  ])
  
(defn board-planets []
  (doall (for [p @planets]
      (if (nil? (:winner p))  ; Only show Active Planets
        ^{:key (:id p)}
          [:div.row.my-2
          ; P1 deployment
            [:div.col-sm-4
              (for [crd (->> @deck :decklist (filter #(= (:pos %) (:id p))))]
                (cardimgwrap crd))
            ; Command Icons  
              [:div.command
                (for [n (range (->> @deck :decklist (filter #(= (:pos %) (:id p))) (map :command_icons) (remove nil?) (reduce +)))]
                  ^{:key n}[:span [:i.fas.fa-gavel.fa-sm.command-icon]])]]
          ; Planets  
            [:div.col-sm-4
              [:div [:img.card-planet {:src (devimg (:img p)) 
                                    :alt (if (:revealed p) (:name p) "Planet")
                                    :on-mouse-over #(swap! appstate assoc :img (:img p))}]]]
          ; P2 deployment 
            [:div.col-sm-4]]))))
          
(defn board-log []
  [:div#log
    (for [msg @log] msg)])
          
(defn renderpage []
  [:div.container-fluid
    [:div.row
      [:div.col-sm-9
        [hq]
        (board-planets)]
      [:div.col-sm-3
        [:div.row
          [:div.cardimg.w-100 [:img.img-fluid.float-right {:hidden (nil? (:img @appstate)) :src (devimg (:img @appstate))}]]]
        [:div.row
          (board-log)]]]
    [ctxmenu]
  ])

; (defn ^:export main []
  (do-setup)
  (r/render [renderpage] (.getElementById js/document "app"))
;)