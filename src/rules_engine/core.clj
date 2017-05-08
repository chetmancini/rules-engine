(ns rules-engine.core
  (:require [clojure.set :refer [difference subset? union]])
  (:gen-class))

(defn -main [& args]
  (println "Not supported"))

(def graph [])

(defn get-rule-inputs [rule]
  (set (first rule)))

(defn get-rule-output [rule]
  (last rule))

(defn- flatten-sets
  "Like flatten, but pulls elements out of sets instead of sequences."
  [v]
  (filter (complement set?)
          (rest (tree-seq set? seq (set v)))))

(defn rotate [v n]
  (let [cv (count v), n (mod n cv)]
    (concat (subvec v n cv) (subvec v 0 n))))

(defn- get-all-outputs [rules]
  (into #{} (map get-rule-output rules)))

(defn partition-rules
  [rules]
  (let [all-inputs (into #{} (flatten-sets (map get-rule-inputs rules)))
        all-outputs (get-all-outputs rules)]
    [(difference all-inputs all-outputs) all-outputs]))

(defn can-infer? [rule assertions]
  (subset? (get-rule-inputs rule) (set assertions)))

(defn infer [rule assertions]
  (if (can-infer? rule assertions)
                 (conj assertions (get-rule-output rule))
                 assertions))

(defn run-inference
  "Recurison easy with assertions as the accumulator, but head may need to be swapped to guarantee completion
  Failure modes not implemented"
  [rules assertions]
  (if (empty? rules)
    assertions
    (if (can-infer? (first rules) assertions)
      (run-inference (rest rules) (infer (first rules) assertions))
      (run-inference (concat (rest rules) [(first rules)]) assertions))))

(defn rules-to-map [rules]
  (into {} (map (fn [rule] [(get-rule-output rule) (get-rule-inputs rule)]) rules)))

(defn traverse [rulemap candidates acc]
  (if (empty? candidates)
    acc
    (let [candidate (first candidates)
          is-output? #(contains? rulemap %)]
      (if (is-output? candidate)
        (traverse rulemap (union (disj candidates candidate) (get rulemap candidate)) acc)
        (traverse rulemap (disj candidates candidate) (conj acc candidate))))))

(defn get-smallest-inputs [rules output]
  (let [rulemap (rules-to-map rules)]
    (traverse rulemap (get rulemap output) [])))

(defn get-input-mapping [rules]
  (into {} (map (fn [output]
                  [output (get-smallest-inputs rules output)])
                (get-all-outputs rules))))