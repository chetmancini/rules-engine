(ns rules-engine.core-test
    (:require [clojure.test :refer :all]
      [rules-engine.core :refer [get-rule-inputs
                                 get-rule-output
                                 partition-rules
                                 infer
                                 run-inference
                                 rules-to-map
                                 get-input-mapping]]))

(def basic-rules [[[:a :b] :-> :c]
                  [[:c :d :e] :-> :f]])

(deftest test-get-rule-inputs
         (testing "gets the inputs to a rule"
                  (is (= #{:a :b} (get-rule-inputs [[:a :b] :-> :c])))))


(deftest test-get-rule-output
         (testing "gets the output to a rule"
                  (is (= :c (get-rule-output [[:a :b] :-> :c])))))

(deftest test-partition
         (testing "partitions inputs/outputs"
                  (is (= [#{:e :b :d :a} #{:c :f}] (partition-rules [[[:a :b] :-> :c] [[:c :d :e] :-> :f]])))))

(deftest test-infer
         (testing "run inference with one rule"
                  (is (= #{:a :b :c} (infer [[:a :b] :-> :c] #{:a :b})))))

(deftest test-run-inference
         (testing "run inference on a set of rules"
                  (is (= #{:a :b :c :d :e :f} (run-inference [[[:a :b] :-> :c]
                                                              [[:c :d :e] :-> :f]]
                                                             #{:a :b :d :e}))))
         (testing "run inference works inverted"
                  (is (= #{:a :b :c :d :e :f} (run-inference [[[:c :d :e] :-> :f]
                                                              [[:a :b] :-> :c]]
                                                             #{:a :b :d :e})))))

(deftest test-rules-to-map
         (testing "rules to map"
                  (is (= {:c #{:a :b}
                          :f #{:c :d :e}}
                         (rules-to-map basic-rules)))))

(deftest test-get-input-mapping
         (testing "get all smallest inputs for all outputs"
                  (is (= {:c [:b :a]
                          :f [:e :b :d :a]} (get-input-mapping basic-rules)))))
