(ns gql-format.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [gql-format.core :as gf]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

; qualify tests
(deftest simple-qualifier
  (testing "qualify a simgle symbol"
    (is (= ::gf/symbol (gf/qualify ?symbol)))))

(deftest flat-map-qualifier
  (testing "qualify a flat map"
    (is (= {:key ::gf/value} (gf/qualify {:key ?value})))))

(deftest nested-map-qualifier
  (testing "qualify a nested map"
    (is (= {:k2 {:key ::gf/value} :unrelated :value}
           (gf/qualify {:k2 {:key ?value} :unrelated :value})))))

(deftest multiple-qualifiers
  (testing "qualify multiple symbols in a map"
    (is (= {:k1 ::gf/v1 :k2 ::gf/v2}
           (gf/qualify {:k1 ?v1 :k2 ?v2})))))


; build tests
(deftest build-flat-query
  (testing "build a query from the simplest output shape"
    (is (= (gf/build (gf/qualify {"field1" ?param1 "field2" ?_}))
           "{field1 field2}"))))

(deftest build-nested-query
  (testing "build a query with nested fields"
    (is (= (gf/build (gf/qualify
                       {"field1" ?param1
                        "field2" ?_
                        "field3" {"nested1" ?n1
                                  "nested2" ?n2}}))
           "{field1 field2 field3{nested1 nested2}}"))))

(deftest build-argument-query
  (testing "build a query with argument fields"
    (is (= (gf/build (gf/qualify
                       {"field1" ?param1
                        "field2" {:arg1 "val1"
                                  :arg2 3
                                  "nested1" ?n1
                                  "nested2" ?n2}}))
           "{field1 field2(arg1: val1, arg2: 3){nested1 nested2}}"))))

(deftest build-list-query
  (testing "build a query containing a list field"
    (is (= (gf/build (gf/qualify
                       {"field1" ?param1
                        "field2" [:arg1 3
                                  :arg2 "hi"
                                  {"field3" ?_}]}))
           "{field1 field2(arg1: 3, arg2: hi){field3}}"))))

(deftest build-as-query
  (testing "build a query with ?as bindings"
    (is (= (gf/build (gf/qualify
                       {?as ?all
                        "field1" [?as ?list
                                  {?as ?list-item
                                   "field2" ?val}]}))
           "{field1{field2}}"))))

; extract bindings tests
(deftest extract-flat-map
  (testing "extract qualified symbols from a flat map"
    (is (= (gf/last-bindings (gf/qualify
                               {"field1" ?val1
                                "field2" ?val2}))
           {::gf/val1 ["field1"]
            ::gf/val2 ["field2"]}))))

(deftest extract-nested-map
  (testing "extract qualified symbols from a nested map"
    (is (= (gf/last-bindings (gf/qualify
                               {"field1" ?val1
                                "field2" {"field3" ?val2}}))
           {::gf/val1 ["field1"]
            ::gf/val2 ["field2" "field3"]}))))

(deftest extract-list-param
  (testing "extract qualified symbol within a vector"
    (is (= (gf/last-bindings (gf/qualify
                               {"field1" ?val1
                                "field2" [?val2]}))
           {::gf/val1 ["field1"]
            ::gf/val2 ["field2" ::gf/list]}))))

(deftest extract-as-symbol
  (testing "extract qualified symbols from a nested map with ?as"
    (is (= (gf/last-bindings (gf/qualify
                               {"field1" ?val1
                                "field2" {?as ?val2
                                          "field3" ?val3}}))
           {::gf/val1 ["field1"]
            ::gf/val2 ["field2"]
            ::gf/val3 ["field2" "field3"]}))))

(deftest unique-symbol
  (testing (str "parse-bindings should throw assertion error when"
                " same symbol is used multiple times.")
    (is (try (nil? (gf/parse-output (gf/qualify
                                      {"field1" ?val1
                                       "field2" ?val1})))
             (catch AssertionError _ true)))))

(deftest convert-flat-map
  (testing "convert flat data from flat format to flat format"
    (is (= (gf/convert (-> {"field1" ?val1
                            "field2" ?val2}
                           gf/qualify
                           gf/parse-output)
                       (gf/qualify {?val1 ?val2})
                       {"field1" 1
                        "field2" 2})
           {1 2}))))

(deftest convert-nested-data
  (testing "convert nested data to a flat format"
    (is (= (gf/convert (-> {"field1" ?val1
                            "field2" {"field3" ?val2}}
                           gf/qualify
                           gf/parse-output)
                       (gf/qualify {?val1 ?val2})
                       {"field1" 1
                        "field2" {"field3" 2}})
           {1 2}))))

(deftest convert-nested-format
  (testing "convert nested data to a nested format"
    (is (= (gf/convert (-> {"field1" ?val1
                            "field2" {"field3" ?val2}}
                           gf/qualify
                           gf/parse-output)
                       (gf/qualify {:a ?val1
                                    :b {:c ?val2}})
                       {"field1" 1
                        "field2" {"field3" 2}})
           {:a 1 :b {:c 2}}))))

(deftest convert-list-binding
  (testing "convert data containing list binding"
    (is (= (-> {"f1" ?v1 "f2" [?v2]}
               gf/qualify gf/parse-output
               (gf/convert (gf/qualify [?for [?v2] ?v2])
                           {"f1" 1 "f2" [2 3 4]}))
           [2 3 4]))))
