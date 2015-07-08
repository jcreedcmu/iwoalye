(ns iwoalye.prod
  (:require [iwoalye.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
