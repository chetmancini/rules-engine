(ns rules-engine.core
  (:require [clojure.set :refer [difference subset? union]]
            [clojure.math.combinatorics :as combo])
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

(defn- get-all-outputs [rules]
  (into #{} (map get-rule-output rules)))

(defn partition-rules
  "Given a set of rules, return both the inputs and outputs"
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

(defn- run-inference-safe
  [rules assertions guard]
  (if (or (empty? rules) (= guard (count rules)))
    assertions
    (if (can-infer? (first rules) assertions)
      (run-inference-safe (rest rules) (infer (first rules) assertions) 0)
      (run-inference-safe (concat (rest rules) [(first rules)]) assertions (+ guard 1)))))

(defn run-inference
  "Recurison easy with assertions as the accumulator, but head may need to be swapped to guarantee completion.
  A guard makes sure we don't get infinite recursion"
  [rules assertions]
  (run-inference-safe rules assertions 0))

(defn rules-to-map [rules]
  (into {} (map (fn [rule] [(get-rule-output rule) (get-rule-inputs rule)]) rules)))

(defn- traverse [rulemap candidates acc]
  (if (empty? candidates)
    acc
    (let [candidate (first candidates)
          is-output? #(contains? rulemap %)]
      (if (is-output? candidate)
        (traverse rulemap (union (disj candidates candidate) (get rulemap candidate)) acc)
        (traverse rulemap (disj candidates candidate) (conj acc candidate))))))

(defn get-fewest-inputs
  "Given a set of rules, what are the fewest inputs that can determine a given output"
  [rules output]
  (let [rulemap (rules-to-map rules)]
    (traverse rulemap (get rulemap output) [])))

(defn get-input-mapping
  "Show all sets of fewest inputs for all outputs in rules."
  [rules]
  (into {} (map (fn [output]
                  [output (get-fewest-inputs rules output)])
                (get-all-outputs rules))))

(defn- make-combinations [inputs]
  (->> inputs
       (combo/subsets)
       (remove empty?)
       (map set)))

(defn all-combinations-inferences [rules]
  (let [safe-inference (fn [inputs]
                         (difference (run-inference rules inputs) inputs))]
    (into {} (map (fn [in-subset] [in-subset (safe-inference in-subset)])
                  (make-combinations (vec (first (partition-rules rules))))))))
