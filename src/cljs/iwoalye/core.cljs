(ns iwoalye.core
    (:require [reagent.core :as reagent :refer [atom cursor]]
              [reagent.session :as session]
              [goog.events :as events]
              [goog.history.EventType :as EventType])
    (:import goog.History))

(enable-console-print!)

; from http://stackoverflow.com/questions/27602592/reagent-component-did-mount
(def initial-focus
  (with-meta identity
    {:component-did-mount #(.focus (reagent/dom-node %))}))

(def template [0 1
               1 2
               2 1
               0 3
               2 2
               3 0])

(defn match-all [text re]
  (let [x #js []
        gre (js/RegExp. (.-source re) "g")]
    (loop [] (if-let [m (.exec gre text)]
               (do (.push x #js {:match m :index (.-index m)}) (recur))
               x))))

(defn brackets [text]
  (js->clj (match-all text #"\[(.*?)\]")))

(defn error [state]
  (let [qas (:qas state)
        num-qas (count qas)
        target (template (dec num-qas))
        actual (count (brackets (last qas)))]
    (if (= actual target) nil (str "This " (if (= 1 (mod num-qas 2)) "question" "answer")
                                   " needs to have "
                                   target
                                   " [bracketed] "
                                   (if (= 1 target) "name" "names") ".")))
  )

(defn ritem [satm n]
  (let [state @satm
        qatm (cursor satm [:qas])
        qs (:qas state)
        x (get qs n)]
    (if (not= (inc n) (count qs)) [:span x]
        [initial-focus
         [:textarea
          {:style {:resize "none" :width "100%" :height "100%"}
           :on-change #(reset! (cursor qatm [n]) (-> % .-target .-value))
           :on-key-press (fn [e] (if (= 13 (.-charCode e))
                                   (do
                                     (when (not (error state))
                                       (swap! qatm (fn [x] (pr ) (vec (concat x [""] )))))
                                     (.preventDefault e))
#_                                   (print (.-charCode e))))
           :value x}]])))




(defn error-comp [state] (if-let [err (error state)] [:div.error err] [:span]))
(def qas (atom { :qas ["bloop [blorp]"  ""]}))

(defn interface [satm]
  (let [state @satm
        qas (:qas state)
        num-qas (count qas)
        num-full-rows (int (/ num-qas 2))]
    [:span [:table (for [n (range num-full-rows)]
                     ^{:key n} [:tr
                                [:td [ritem satm (* 2 n)]]
                                [:td [ritem satm (inc (* 2 n))]]
                                ])
            (if (= 1 (mod num-qas 2)) [:tr [:td [ritem satm (dec num-qas)]]])]
     [error-comp state]
     [:span (pr-str state)]]))

;; -------------------------
;; Initialize app
(defn mount-root []
  (reagent/render [interface qas] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
