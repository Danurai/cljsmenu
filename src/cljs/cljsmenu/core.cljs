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

; Initalisation and deck fns
(defn- getcard [code]  
  (->> @cards 
      (filter #(= (:code %) code))
      first))

(defn logaction [msg]
  (prn msg)
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
  
(defn drawcard []
  (let [drawid (->> @deck :decklist (filter #(= (:pos %) :deck)) first :idx)]
    (swap! deck assoc :decklist (map #(if (= (:idx %) drawid) (assoc % :pos :hand) %) (:decklist @deck)))
    (logaction "Draw Card.")))
  
(defn build-deck [rawdeck]
  (let [all-cards (->> (map (fn [x] (repeat (x rawdeck) (getcard (name x)))) (keys rawdeck)) (reduce concat))]
    (reset! deck {:warlord (->> all-cards (filter #(= (:type_code %) "warlord_unit")) first)
                :resources (->> all-cards (filter #(= (:type_code %) "warlord_unit")) first :starting_resources)
                :decklist (map-indexed #(assoc %2 :idx %1 :pos :deck) (filter #(not= (:type_code %) "warlord_unit") all-cards))})
    (shuffledeck)
    (dotimes [n (-> @deck :warlord :starting_hand)] (drawcard))))

(defn do-setup  []
  (go 
    (reset! cards (:data (js->clj (.parse js/JSON (<! (GET "http://localhost:9009/api/data/cards"))) :keywordize-keys true)))
    (build-planets)
    (build-deck  {:010006 1 :010123 4 :010124 1 :010125 2 :010126 1 :010128 1 :010127 1 :010137 1 :010129 2 :010133 1 :010134 2 :010136 1 :010131 1 :010138 1 :010135 1 :010132 1 :010130 1 :010171 1 :010170 1 :010174 1 :010172 1 :010169 1 :010173 1 :010143 1 :010145 1 :010144 1 :010139 1 :010140 1 :010142 1 :010141 1})
    (reset! log ())
))


(defn deploycard [card-id planet-id]
  (let [crd (->> @deck :decklist (filter #(= (:idx %) card-id)) first)
       plnt (->> @planets (filter #(= (:id %) planet-id)) first)]
    (swap! deck update :resources - (:cost crd 0))
    (swap! deck assoc :decklist (map #(if (= (:idx %) card-id) (assoc % :pos planet-id) % ) (:decklist @deck)))
    (logaction [:span [:a {:href "#" :on-mouse-over #(swap! appstate assoc :img (:img crd))} (:name crd)] (str " deployed to " (:name plnt))])))

(defn toggle-popupmenu [e]
  (prn @menu)
  (swap! menu assoc :hidden (->> @menu :hidden ((complement true?)))
                  :left (str (-> e .-pageX (- 20)) "px")
                  :top (str (-> e .-pageY (- 20)) "px")))
(defn- close-popupmenu [] (swap! menu assoc :hidden true))

; Controller Functionality

(defn deck-click [e]
  (swap! menu assoc :buttons [{:title "Draw"    :data-action drawcard}
                           {:title "Shuffle" :data-action shuffledeck}
                           {:title "dropdown-divider"}])
  (toggle-popupmenu e))
    
(defn hand-click [idx e]
  (let [card (->> @deck :decklist (filter #(= (:idx %) idx)) first)]
    (when (<= (:cost card 0) (:resources @deck))
      (swap! menu assoc :buttons 
        (case (:type_code card) 
          "army_unit"  (remove nil? (map #(if (:revealed %) {:title (:name %) :data-action (fn [] (deploycard (:idx card) (:id %)))}) @planets))
          "attachment" (remove nil? (map #(if (:revealed %) {:title (:name %) :data-action (fn [] (deploycard (:idx card) (:id %)))}) @planets))
          "event"     [{:title "Play"}]
          "support"   [{:title "Play"}]
          nil))
      (toggle-popupmenu e))))



; Page Layout    

(defn ctxmenu []
  [:div#popupmenu.popupmenu {:hidden (:hidden @menu) 
                          :style {:left (:left @menu) :top (:top @menu)}}
    ;[:button.close {:on-click #(close-popupmenu)} "\u00D7"]
    ;[:div.popupmenu-header (:title @menu)]
    [:div.btn-group.btn-group-sm.btn-group-vertical.d-flex
      (for [button (:buttons @menu)]
        ^{:key (:title button)}
          [:button.btn.btn-block.btn-light 
            {:on-click #((:data-action button)(close-popupmenu))}
            (:title button)])]])

(defn hq []
  [:div#hq
    [:div "HQ"]
    [:div
      [:div#deck.card-wrap
        [:img.card {:src "/img/cardback.png" :alt "deck" :on-click #(deck-click %)}] ;:src "/img/cardback.png"
        [:span.cardcount (->> @deck :decklist (filter #(= (:pos %) :deck)) count)]]
      [:div.card-wrap
        [:img.card {:alt "warlord" :src (-> @deck :warlord :img)}]] ;(-> @deck :warlord :img)}]
      [:div.card-wrap.resources 
        [:span.mx-2 [:i.fas.fa-cog.fa-xs.mr-2] (:resources @deck)]
        [:span.btn-sm{:on-click #(swap! deck update :resources dec)} "-"]
        [:span.btn-sm {:on-click #(swap! deck update :resources inc)} "+"]]]
    [:div (str "Hand (" (->> @deck :decklist (filter #(= (:pos %) :hand)) count) ")")]
    [:div
      (doall (for [r (->> @deck :decklist (filter #(= (:pos %) :hand)))]
        ^{:key (:idx r)}
          [:div.card-wrap {:class (if (<= (:cost r 0) (-> @deck :resources)) "can-play" "no-play") }
            [:img.card {:src (:img r)
                        :alt (:name r)
                        :on-click #(hand-click (:idx r) %)
                        ;:on-mouse-out #(swap! appstate assoc :img nil)
                        :on-mouse-over #(swap! appstate assoc :img (:img r))}]]))]  ;(:img r)
  ])

(defn board-planets []
  (doall
    (for [p @planets]
      (if (nil? (:winner p))
        ^{:key (:id p)}
          [:div.row.my-2
            [:div.col-sm-4
              (for [crd (->> @deck :decklist (filter #(= (:pos %) (:id p))))]
                ^{:key (:idx crd)}[:div.card-wrap [:img.card {:src (:img crd) :alt (:name crd) :on-mouse-over #(swap! appstate assoc :img (:img crd))}]])
              [:div.command (repeat (->> @deck :decklist (filter #(= (:pos %) (:id p))) (map :command_icons) (remove nil?) (reduce +)) [:i.fas.fa-gavel.fa-sm.command-icon])]] ; [:span.command-icon "C"])]]
            [:div.col-sm-4
              [:div [:img.card-planet {:alt (if (:revealed p) (:name p) "Planet") :src (:img p) :on-mouse-over #(swap! appstate assoc :img (:img p))}]]] ;:img (:img p)
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
          [:div.cardimg.w-100 [:img.img-fluid.float-right {:hidden (nil? (:img @appstate)) :src (:img @appstate)}]]]
        [:div.row
          (board-log)]]]
    [ctxmenu]
  ])

; (defn ^:export main []
  (do-setup)
  (r/render [renderpage] (.getElementById js/document "app"))
;)