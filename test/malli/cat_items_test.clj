(ns malli.cat-items-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]))

(deftest an-item-cat-validates-exactly-as-its-regex-explainer
  (let [registry (merge (m/default-schemas)
                        {::pair [:cat :int :int]
                         ::n :int})
        options {:registry registry}
        values [[1 :k {:a 1}] [1 :k {:a :x}] [1 :k] [1 :k {:a 1} 4] []
                '(1 :k {:a 1}) (list 1 :k) (map identity [1 :k {:a 1}])
                (lazy-seq (cons 1 (lazy-seq [:k {:a 1}]))) (seq [1 :k {:a 1}])
                nil {:a 1} "1:k" #{1}]]
    (doseq [schema [[:cat :int :keyword [:map [:a :int]]]
                    [:cat]
                    [:cat ::n :keyword [:map [:a :int]]]
                    [:cat [:schema [:cat :int]] :keyword [:map [:a :int]]]]
            value values]
      (testing (pr-str schema value)
        (is (= (nil? (m/explain schema value options))
               ((m/validator schema options) value)))))
    (testing "a nested regex operator keeps splicing"
      (is ((m/validator [:cat ::pair :keyword] options) [1 2 :k]))
      (is (not ((m/validator [:cat ::pair :keyword] options) [[1 2] :k])))
      (is ((m/validator [:cat :int [:? :int]] options) [1])))
    (testing "a :ref child is still refused as a potentially recursive seqex"
      (is (thrown-with-msg? Exception #"potentially-recursive-seqex"
                            ((m/validator [:cat [:ref ::n]] options) [1]))))))

(deftest an-instrumented-call-reports-its-arguments-as-a-vector
  (let [reports (atom [])
        report (fn [kind data] (swap! reports conj [kind data]))
        zero (m/-instrument {:schema [:=> [:cat] :int] :report report} (fn [] 1))
        one (m/-instrument {:schema [:=> [:cat :int] :int] :report report} (fn [x] (str x)))
        guarded (m/-instrument {:schema [:=> [:cat :int] :int [:fn (fn [[args ret]] (= ret (first args)))]]
                                :report report}
                               (fn [x] (inc x)))]
    (is (= 1 (zero)))
    (is (empty? @reports))
    (one :x)
    (guarded 1)
    (is (= [[::m/invalid-input [:x]] [::m/invalid-output [:x]] [::m/invalid-guard [1]]]
           (mapv (fn [[kind data]] [kind (:args data)]) @reports)))
    (is (every? (comp vector? :args second) @reports))))
