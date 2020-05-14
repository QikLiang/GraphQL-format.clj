(ns gql-format.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [gql-format.core :as gf]))

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

(deftest all-symbol-instances
  (testing "parse format extracts all instances of all symbols"
    (is (= (gf/parse-format (gf/qualify
                              {"field1" ?val1
                               "field2" {"field3" ?val1
                                         "field4" ?val2}}))
           {["field1"] ::gf/val1
            ["field2" "field3"] ::gf/val1
            ["field2" "field4"] ::gf/val2}))))
