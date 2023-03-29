(ns budget-manager.core
  (:gen-class :main true)
  (:require [clojure.edn :as edn]))

(def categories
  {"rent" nil
   "sports and leisure" nil
   "ultilities" nil
   "transportation" nil
   "restaurants" nil
   "clothing and shoes" nil
   "markets" nil})

(defn check-q 
  [ ask-str]
  (do 
    (println ask-str)
    (let [value (read-line)]
      (if (= value "N")
        value
        (if (number? (edn/read-string value))
          (edn/read-string value)
          (recur  ask-str))))))

(defn loop-pre-budget
  ([]
   (loop-pre-budget categories {}))
  ([remaining result]
   (if (= 0 (count remaining))
     result
     (let [current-key (first (first remaining))
           ask-str (str "Have you already allocated money for "
                        current-key
                        "? "
                        "Type in the value or type 'N' if you haven't ")]
       (let [value (check-q ask-str)]
         (if (= value "N")
           (recur (dissoc remaining current-key) result )
           (recur (dissoc remaining current-key) (assoc result current-key value))
           ))))))

(defn loop-post-budget
  [remaining budget new-plan weeks]
  (if (= 0 (count remaining))
    new-plan
    (let [current-key (first (first remaining))
          ask-str (str "How much will you allocate for " current-key "?")]
      (println (str "You have " (str budget)))
      (let [value (/ (check-q ask-str) weeks)
            next-plan (assoc new-plan current-key value)]
        (recur (dissoc remaining current-key)
               (- budget (* value weeks))
               next-plan
               weeks)))))



(defn account-for-budget
  [budget preplanned weeks]
  (let [new-budget (- budget (apply + (map second (seq preplanned))))
        remaining (apply dissoc categories (map first (seq preplanned)))]
    (loop-post-budget remaining new-budget {} weeks)))

(defn print-results
  [results]
  (if (> (count results) 0)
    (let [key (first (first results))
          value (second (first results))]
      (println (str key " : " value))
      (recur (dissoc results key)))))

(defn -main 
  []
  (let [
        weeks (check-q "How many weeks do you want to budget for?")
        budget (check-q "How much of a budget do you have for the time frame?")
        preplanned (loop-pre-budget)
        results (account-for-budget budget preplanned weeks)]
    (println "The following shows your budget")
    (print-results results) 
    ))




