(ns iwoalye.core
    (:require [reagent.core :as reagent :refer [atom cursor]]
              [reagent.session :as session]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [clojure.core.reducers :refer [reduce]])
    (:import goog.History))

(enable-console-print!)

; from http://stackoverflow.com/questions/27602592/reagent-component-did-mount
(def initial-focus
  (with-meta identity
    {:component-did-mount #(.focus (reagent/dom-node %))}))

(def tgt-qa-counts [1 1
                1 2
                2 1
                0 3
                2 2
                3 0])

(def tgt-term-counts [2 5 4 1 3 1 2])

(defn match-all [text re]
  (let [x #js []
        gre (js/RegExp. (.-source re) "g")]
    (loop [] (if-let [m (.exec gre text)]
               (do (.push x #js {:match m :index (.-index m)}) (recur))
               x))))

(defn brackets [text]
  (js->clj (match-all text #"\[(.*?)\]") :keywordize-keys true))

(defn replace-brackets [text]
  (let [split (.split text #"\[(.*?)\]")]
    (vec (concat [:span] (map-indexed (fn [ix v] (if (= 1 (mod ix 2)) [:span.bracketed v] v)) split)))))

;; http://stackoverflow.com/questions/4830900/how-do-i-find-the-index-of-an-item-in-a-vector
(defn positions
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn get-term-counts [qas]
  (let [terms  (map #((:match %) 1) (apply concat (map brackets qas)))]
    (reduce
     (fn [acc b]
       (let [ps (positions #(= b (:term %)) acc)]
         (if (empty? ps)
           (vec (concat acc [{:term b :count 1}]))
           (update-in acc [(first ps) :count] inc)
           ))) []
     terms)))

(defn error [state]
;  nil #_
  (let [raw-qas (:qas state)
        qas (take (count tgt-qa-counts) raw-qas)
        counts (get-term-counts qas)
        num-qas (count qas)
        target (tgt-qa-counts (dec num-qas))
        actual (count (brackets (last qas)))]
    (or
     (if (= actual target) nil (str "This " (if (= 1 (mod num-qas 2)) "question" "answer")
                                    " needs to have "
                                    target
                                    " [bracketed] "
                                    (if (= 1 target) "name" "names") "."))
     (if (> (count counts) (count tgt-term-counts))
       "Too many distinct bracketed terms; re-use an already mentioned one instead.")
     (some #(do %) (map-indexed (fn [ix v] (if (> (:count v) (tgt-term-counts ix))
                                             (str "Too many occurrences of [" (:term v) "]"))) counts))
     )))

(defn ritem [satm n]
  (let [state @satm
        qatm (cursor satm [:qas])
        qs (:qas state)
        x (get qs n)]
    (if (not= (inc n) (count qs)) [:span (replace-brackets x)]
        [initial-focus
         [:textarea
          {:style {:resize "none"}
           :on-change #(reset! (cursor qatm [n]) (-> % .-target .-value))
           :on-key-press (fn [e] (if (= 13 (.-charCode e))
                                   (do
                                     (when (not (error state))
                                       (swap! qatm (fn [x] (vec (concat x [""] )))))
                                     (.preventDefault e))
#_                                   (print (.-charCode e))))
           :value x}]])))

(defn error-comp [state] (if-let [err (error state)] [:div.error err] [:span]))

(def state (atom {}))
(defn init-state []
  (reset! state { :qas
                 ["Why did the [Iwo] die out?"  ""]}))

(defn terms-comp [qas]
  (let [counts (get-term-counts qas)]
    [:div.terms (map-indexed (fn [ix c]
                           ^{:key ix}
                           [:div.story-row
                            [:div.story-item (:term c)]
                            [:div.story-item (:count c)
                             (if (< ix (count tgt-term-counts))
                               (str "/" (tgt-term-counts ix))
                               "")]])
                         counts)]))

(defn interface [satm]
  (let [state @satm
        raw-qas (:qas state)
        finished (>= (dec (count raw-qas)) (count tgt-qa-counts))
        qas (take (count tgt-qa-counts) raw-qas)
        num-qas (count qas)
        num-full-rows (int (/ num-qas 2))]
    [:span
     [:span.flex-container
      [:div.story (for [n (range num-full-rows)]
                    ^{:key n} [:div.story-row
                               [:div.story-item [ritem satm (* 2 n)]]
                               [:div.story-item [ritem satm (inc (* 2 n))]]
                               ])
       (if (and (not finished)
                (= 1 (mod num-qas 2)))
         [:div.story-row
          [:div.story-item [ritem satm (dec num-qas)]]
          [:div.story-item]])]
      (terms-comp qas)
      [error-comp state]]
     (if finished [:button {:on-click init-state} "Consult Another Historian"])]))

;; -------------------------
;; Initialize app

(defn mount-root []
  (init-state)
  (reagent/render [interface state] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
