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

(defn- discard-img [plyr]
  (let [discarded (->> @deck plyr :decklist (filter #(= (:pos %) :discard)) last)]
    (if (nil? discarded) "/img/cardback.png" (:img discarded))))
  
(defn build-planets []
  (reset! planets 
    (take 7
      (map-indexed 
        (fn [idx p] 
          (assoc p :id idx :revealed (< idx 5)))
      (->> @cards (filter #(= (:type_code %) "planet")) shuffle)))))

(defn shuffledeck [plyr]
  (swap! deck assoc-in [plyr :decklist] (-> @deck plyr :decklist shuffle))
  (logaction [:span [:b plyr] ": Shuffle Deck."]))      
  
(defn card-draw [plyr]
  (let [drawid (->> @deck plyr :decklist (filter #(= (:pos %) :deck)) first :idx)]
    (swap! deck assoc-in [plyr :decklist] (map #(if (= (:idx %) drawid) (assoc % :pos :hand) %) (-> @deck plyr :decklist)))
    (logaction [:span [:b plyr] ": Draw Card."])))
    
(defn build-deck [plyr rawdeck]
  (let [all-cards (->> (map (fn [x] (repeat (x rawdeck) (getcard (name x)))) (keys rawdeck)) (reduce concat))]
    (swap! deck assoc plyr {:warlord (->> all-cards (filter #(= (:type_code %) "warlord_unit")) first)
                         :resources (->> all-cards (filter #(= (:type_code %) "warlord_unit")) first :starting_resources)
                         :hp (->> all-cards (filter #(= (:type_code %) "warlord_unit")) first :hp)
                         :bloodied false
                         :decklist (map-indexed #(assoc %2 :idx %1 
                                                       :pos (if (= (:type_code %2) "warlord_unit") :hq :deck)
                                                       :exhausted false) all-cards)})
    (shuffledeck plyr)
    (dotimes [n (-> @deck plyr :warlord :starting_hand)] (card-draw plyr))))

(defn do-setup  []
  (go 
    (reset! cards (:data (js->clj (.parse js/JSON (<! (GET "http://localhost:9009/api/data/cards"))) :keywordize-keys true)))
    (build-planets)
    (build-deck :p1 {:010003 1 :010054 4 :010055 1 :010056 2 :010057 1 :010065 1 :010069 2 :010064 1 :010063 1 :010067 2 :010059 1 :010171 1 :010066 1 :010062 1 :010058 1 :010068 1 :010061 1 :010170 1 :010060 1 :010174 1 :010073 1 :010070 1 :010172 1 :010169 1 :010071 1 :010072 1 :010075 1 :010074 1 :010173 1 :010076 1})
    ;(build-deck :p1 {:010006 1 :010123 4 :010124 1 :010125 2 :010126 1 :010128 1 :010127 1 :010137 1 :010129 2 :010133 1 :010134 2 :010136 1 :010131 1 :010138 1 :010135 1 :010132 1 :010130 1 :010171 1 :010170 1 :010174 1 :010172 1 :010169 1 :010173 1 :010143 1 :010145 1 :010144 1 :010139 1 :010140 1 :010142 1 :010141 1})
    (build-deck :p2 {:010001 1 :010008 4 :010009 1 :010010 2 :010011 1 :010012 1 :010170 1 :010019 1 :010171 1 :010022 1 :010017 1 :010021 1 :010014 1 :010020 1 :010023 1 :010016 1 :010015 2 :010027 1 :010028 1 :010174 1 :010024 1 :010026 1 :010172 1 :010025 1 :010169 1 :010029 1 :010030 1 :010173 1 :010013 2 :010018 1})
    (swap! deck assoc :firstplayer (rand-nth [:p1 :p2]))
    (reset! log ())
))

(defn- hq-phase-player [plyr]
  (dotimes (n 2) (card-draw plyr))            ; Draw 2 cards
  (swap! deck update-in [plyr :resources] + 4) ; Take 4 Resources    
  (swap! deck assoc-in [plyr :decklist]        
    (map #(dissoc % :exhausted)
      (-> @deck plyr :decklist))))            ; Ready all exhausted cards 

(defn hq-phase []
  ; assign next First Planet - in code
  ; Reveal next planet - in code
  ; Draw 2, 4 Res, ready Exhausted
  (hq-phase-player :p1)
  (hq-phase-player :p2)
  ; Pass Initiative
  (swap! deck assoc :firstplayer (if (= (-> @deck :firstplayer) :p1) :p2 :p1)))

(defn card-discard [plyr idx]
  (swap! deck assoc-in [plyr :decklist] 
    (map #(if (= (:idx %) idx) 
             (assoc % :pos :discard)
             %)
      (-> @deck plyr :decklist))))
      
(defn card-hand [plyr idx]
  (swap! deck assoc-in [plyr :decklist] 
    (map #(if (= (:idx %) idx) 
             (assoc % :pos :hand)
             %)
      (-> @deck plyr :decklist))))
      
(defn card-hq [plyr idx]
  (swap! deck assoc-in [plyr :decklist] 
    (map #(if (= (:idx %) idx) 
             (assoc % :pos :hq)
             %)
      (-> @deck plyr :decklist))))
                              
(defn card-deploy [plyr card-id planet-id]
  (let [crd (->> @deck plyr :decklist (filter #(= (:idx %) card-id)) first)
       plnt (->> @planets (filter #(= (:id %) planet-id)) first)]
    (if (= (:pos crd) :hand) (swap! deck update-in [plyr :resources] - (:cost crd 0)))
    (swap! deck assoc-in [plyr :decklist] (map #(if (= (:idx %) card-id) (assoc % :pos planet-id) % ) (-> @deck plyr :decklist)))
    (logaction [:span [:b plyr] ": " [:a {:href "#" :on-mouse-over #(swap! appstate assoc :img (:img crd))} (:name crd)] (str " deployed to " (if (some? plnt) (:name plnt) "HQ"))])))

(defn card-play [plyr card-id]
  (let [crd (->> @deck plyr :decklist (filter #(= (:idx %) card-id)) first)]
    (swap! deck update-in [plyr :resources] - (:cost crd 0))
    (card-discard plyr card-id)
    (logaction [:span [:b plyr] " played " [:a {:href "#" :on-mouse-over #(swap! appstate assoc :img (:img crd))} (:name crd)]])))
    
(defn card-toggle-exhaust [plyr card-id] 
  (swap! deck assoc-in [plyr :decklist] 
    (map #(if (= (:idx %) card-id) (assoc % :exhausted (-> % :exhausted true? not)) %) (-> @deck plyr :decklist) )))
(defn card-warlord-bloodied [plyr]
  (swap! deck assoc-in [plyr :bloodied] (-> @deck plyr :bloodied true? not)))
    
(defn card-dmg [plyr crd-id fun]
  (swap! deck assoc-in 
    [plyr :decklist]
    (map #(if (= (:idx %) crd-id)
             (assoc % :dmg (max 0 (fun (:dmg %))))
             %)
      (-> @deck plyr :decklist))))
    
(defn card-counter [plyr crd-id fun]
  (swap! deck assoc-in 
    [plyr :decklist]
    (map #(if (= (:idx %) crd-id)
             (assoc % :counter (max 0 (fun (:counter %))))
             %)
      (-> @deck plyr :decklist))))

(defn card-planet-winner [id plyr]
  (reset! planets 
    (map #(if (= (:id %) id)
             (assoc % :winner plyr)
             %)
      @planets))
  (let [next-plnt (->> @planets (filter #(false? (:revealed %))) first :id)]
    (reset! planets
      (map #(if (= (:id %) next-plnt)
               (assoc % :revealed true)
               %)
        @planets))))
      
(defn toggle-popupmenu [e]
  (swap! menu assoc :hidden (->> @menu :hidden ((complement true?)))
                  :left (str (-> e .-pageX (- 20)) "px")
                  :top (str (-> e .-pageY (- 20)) "px")))
(defn- close-popupmenu [] (swap! menu assoc :hidden true))




; Controller Functionality

(defn deck-click [plyr e]
  (swap! menu assoc :buttons [{:title "Shuffle" :data-action #(shuffledeck plyr)}])
  (toggle-popupmenu e))

(defn discard-click [e]
  (prn e))
  
(defn hand-click [plyr idx e]
  (let [card (->> @deck plyr :decklist (filter #(= (:idx %) idx)) first)
       planets-available (->> @planets (filter #(true? (:revealed %))) (filter #(nil? (:winner %))))]
    (if (<= (:cost card 0) (-> @deck plyr :resources))
      (swap! menu assoc :buttons
        (concat 
          (case (:type_code card) 
            "army_unit"  (remove nil? (map #(if (nil? (:owner %)) {:title (:name %) :data-action (fn [] (card-deploy plyr (:idx card) (:id %)))}) planets-available))
            "attachment" (remove nil? (map #(if (:revealed %) {:title (:name %) :data-action (fn [] (card-deploy plyr (:idx card) (:id %)))}) planets-available))
            "event"     [{:title "Play" :data-action #(card-play plyr (:idx card))}]
            "support"   [{:title "Deploy to HQ" :data-action #(card-deploy plyr (:idx card) :hq)}]
            nil)
          [{:title "divider"}
           {:title "Discard" :data-action #(card-discard plyr (:idx card))}]))
      (swap! menu assoc :buttons [{:title "Discard" :data-action #(card-discard plyr (:idx card))}]))
    (toggle-popupmenu e)))
      
(defn card-send-to-click [plyr idx e]
  (let [card (->> @deck plyr :decklist (filter #(= (:idx %) idx)) first)]
    (swap! menu assoc :buttons
      (concat 
        (remove nil? (map #(if (:revealed %) {:title (:name %) :data-action (fn [] (card-deploy plyr (:idx card) (:id %)))}) @planets))
        [{:title "divider"}
         {:title "HQ" :data-action #(card-hq plyr (:idx card))}
         {:title "Hand" :data-action #(card-hand plyr (:idx card))}
         ]))
    (toggle-popupmenu e)))

(defn card-click [plyr idx e]
  (let [card (->> @deck plyr :decklist (filter #(= (:idx %) idx)) first)]
    (swap! menu assoc :buttons (concat 
                              (if (= (:type_code card) "warlord_unit")
                                [{:title "Bloodied\\Hale" :data-action #(card-warlord-bloodied plyr)}]
                                [{:title "Discard" :data-action #(card-discard plyr idx)}])
                              [{:title "Exhaust\\Ready" :data-action #(card-toggle-exhaust plyr idx)}
                               {:title "Add Counter" :data-action #(card-counter plyr idx inc)}
                               {:title "Send to..." :data-action #(card-send-to-click plyr idx e)}])))
  (toggle-popupmenu e))
 
(defn card-click-planet [id e]
  (swap! menu assoc :buttons
    [{:title "Winner: p1" :data-action #(card-planet-winner id :p1)}
     {:title "Winner: p2" :data-action #(card-planet-winner id :p2)}])
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
              
(defn cardimgwrap [plyr crd]
    ^{:key (:idx crd)}[:div.whc-card-wrap 
      [:img.whc-card {:class (str
                            (if (true? (:exhausted crd)) "whc-card-tapped ")
                            (if (and (= (:type_code crd) "warlord_unit") (true? (-> @deck plyr :bloodied))) "bloodied "))
                    :src (devimg (:img crd)) 
                    :alt (:name crd)
                    :on-click #(card-click plyr (:idx crd) %)
                    :on-mouse-over #(swap! appstate assoc :img (:img crd))}]
      (if (= (:type_code crd) "army_unit")
        [:div.hp
          [:div.whc-btn {:on-click #(card-dmg plyr (:idx crd) inc)} [:i.fas.fa-angle-up]]
          [:span (:dmg crd 0)]
          [:div.whc-btn {:on-click #(card-dmg plyr (:idx crd) dec)} [:i.fas.fa-angle-down]]])
      (if (some? (:counter crd))
        [:div.counter
          [:div.whc-btn {:on-click #(card-counter plyr (:idx crd) inc)} [:i.fas.fa-angle-up]]
          [:span (:counter crd 0)]
          [:div.whc-btn {:on-click #(card-counter plyr (:idx crd) dec)} [:i.fas.fa-angle-down]]])
      ])
            
(defn hq [plyr]
  [:div.hq {:class (if (= (-> @deck :firstplayer) plyr) "hq-fp")}
    [:div 
      [:span "HQ  "]
      [:span 
        (doall (for [plnt (->> @planets (filter #(= (:winner %) plyr)))]
          ^{:key (:id plnt)}[:span.ml2 (str (:name plnt) ":" (:planet_type plnt))]))]]
    [:div    
      [:div.resources 
        [:div 
          [:span.mx-2 [:i.fas.fa-cog.fa-xs.mr-2] (-> @deck plyr :resources)]
          [:span.btn-res {:on-click #(swap! deck update-in [plyr :resources] dec)} "-"]
          [:span.btn-res {:on-click #(swap! deck update-in [plyr :resources] inc)} "+"]]
        [:div 
          [:span.mx-2 [:i.fas.fa-heart.fa-xs.mr-2] (-> @deck plyr :hp)]
          [:span.btn-res {:on-click #(swap! deck update-in [plyr :hp] dec)} "-"]
          [:span.btn-res {:on-click #(swap! deck update-in [plyr :hp] inc)} "+"]]
        [:div.m-1
          [:button.btn.btn-block.btn-outline-light.btn-sm
            {:on-click #(card-draw plyr)} "Draw"]]
        [:div.m-1
          [:button.btn.btn-block.btn-outline-light.btn-sm
            {:on-click #(hq-phase)} "HQ Phase"]]]
        [:div#discard.whc-card-wrap
        [:img.whc-card {:src (-> plyr discard-img devimg) :alt "discard" :on-click discard-click}]
        [:span.whc-cardcount (->> @deck plyr :decklist (filter #(= (:pos %) :discard)) count)]]
      [:div#deck.whc-card-wrap
        [:img.whc-card {:src (devimg "/img/cardback.png") :alt "deck" :on-click #(deck-click plyr %)}]
        [:span.whc-cardcount (->> @deck plyr :decklist (filter #(= (:pos %) :deck)) count)]]
      ;[:div#warlord.whc-card-wrap
      ;  [:img.whc-card {:src (devimg (-> @deck plyr :warlord :img)) 
      ;                :alt (str "Warlord: "  (-> @deck plyr :warlord :name))
      ;                :on-mouse-over #(swap! appstate assoc :img (-> @deck plyr :warlord :img))}]]
      [:div.hq-deployed
        (doall 
          (for [crd (->> @deck plyr :decklist (filter #(= (:pos %) :hq)) (sort-by :idx))]
            (cardimgwrap plyr crd)))]]
    [:div (str "Hand (" (->> @deck plyr :decklist (filter #(= (:pos %) :hand)) count) ")")]
    [:div
      (doall (for [r (->> @deck plyr :decklist (filter #(= (:pos %) :hand)))]
        ^{:key (:idx r)}
          [:div.whc-card-wrap {:class (if (<= (:cost r 0) (-> @deck plyr :resources)) "can-play" "no-play") }
            [:img.whc-card {:src (devimg (:img r))
                          :title (:name r)
                          :on-click #(hand-click plyr (:idx r) %)
                          ;:on-mouse-out #(swap! appstate assoc :img nil)
                          :on-mouse-over #(swap! appstate assoc :img (:img r))}]]))]])

(defn board-planets []
  [:div#planets.row.mt-4
    (doall (for [p (->> @planets (filter #(nil? (:winner %))) (take 6))]
      ^{:key (:id p)}[:div.col-sm-2
      ; :p1 deployment
        [:div.deployment-zone
          [:div
            (doall (for [crd (->> @deck :p1 :decklist (filter #(= (:pos %) (:id p))))]
              (cardimgwrap :p1 crd)))]
      ; :p1 Command Icons
          [:div
            (for [n (range (->> @deck :p1 :decklist (filter #(= (:pos %) (:id p))) (filter #(false? (:exhausted %))) (map :command_icons) (remove nil?) (reduce +)))]
              ^{:key n}[:span [:i.fas.fa-gavel]])]]
      ; Planet      
        [:div.planet [:img.whc-card-planet {:src (devimg (if (:revealed p) (:img p) "/img/cardback-planet.png")) 
                                        :alt (if (:revealed p) (:name p) "Planet")
                                        :on-click #(card-click-planet (:id p) %)
                                        :on-mouse-over #(swap! appstate assoc :img (:img p))}]]
      ; :p2 Command Icons
        [:div.deployment-zone
          [:div
            (for [n (range (->> @deck :p2 :decklist (filter #(= (:pos %) (:id p))) (filter #(false? (:exhausted %))) (map :command_icons) (remove nil?) (reduce +)))]
              ^{:key n}[:span [:i.fas.fa-gavel]])]
        ; :p2 deployment
          [:div 
            (doall (for [crd (->> @deck :p2 :decklist (filter #(= (:pos %) (:id p))))]
              (cardimgwrap :p2 crd)))]]]
    ))])

(defn board-log []
  [:div#log
    (for [msg @log] msg)])
          
(defn renderpage []
  [:div.container-fluid
    [:div.row
      [:div.col-sm-9 {:style {:user-select "none"}}
        [:ul.nav.nav-tabs
          [:li.nav-item
            [:a.nav-link.active {:href "#p1hq" :data-toggle "tab" :role "tab"} "P1"]]
          [:li.nav-item
            [:a.nav-link {:href "#p2hq" :data-toggle "tab" :role "tab"} "P2"]]]
        [:div.tab-content
          [:div#p1hq.tab-pane.active
            [hq :p1]]
          [:div#p2hq.tab-pane
            [hq :p2]]]
        (board-planets)]
      [:div.col-sm-3
        [:div.row
          [:div.whc-cardimg.w-100 [:img.img-fluid.float-right {:hidden (nil? (:img @appstate)) :src (devimg (:img @appstate))}]]]
        [:div.row
          (board-log)]]]
    [ctxmenu]
  ])

; (defn ^:export main []
  (do-setup)
  (r/render [renderpage] (.getElementById js/document "app"))
;)