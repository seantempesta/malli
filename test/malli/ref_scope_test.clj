(ns malli.ref-scope-test
  (:require [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [malli.registry :as mr]))

(deftest ref-validation-keeps-lookup-scope-without-enumeration
  (let [forms (assoc (m/default-schemas)
                     ::node [:or :string [:vector [:ref ::node]]])
        enumerations (atom 0)
        registry (reify mr/Registry
                   (-schema [_ k] (get forms k))
                   (-schemas [_] (swap! enumerations inc) forms))]
    (doseq [scope [forms registry]]
      (let [valid? (m/validator [:ref ::node] {:registry scope})]
        (is (valid? ["root" ["leaf"]]))
        (is (not (valid? ["root" [42]])))))
    (is (zero? @enumerations))
    (let [valid? (m/validator
                  [:tuple [:ref ::node]
                   [:schema {:registry {::node [:or :int [:vector [:ref ::node]]]}}
                    [:ref ::node]]]
                  {:registry registry})]
      (is (valid? [["outer"] [1 [2]]]))
      (is (not (valid? [[1] [1 [2]]])))
      (is (not (valid? [["outer"] ["inner"]]))))))
