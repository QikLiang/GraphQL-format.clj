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

(deftest build-full-query
  (testing "build a full query with query variables"
    (is (= (-> {"query" {:$arg1 "Int"
                         "field" {:id "$arg1"
                                  "subfield" ?_}}}
               gf/qualify gf/build)
           "query($arg1: Int){field(id: $arg1){subfield}}"))))

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
    (is (= (gf/extract-params (gf/qualify
                                {"field1" ?val1
                                 "field2" ?val2}))
           {::gf/val1 ["field1"]
            ::gf/val2 ["field2"]}))))

(deftest extract-nested-map
  (testing "extract qualified symbols from a nested map"
    (is (= (gf/extract-params (gf/qualify
                                {"field1" ?val1
                                 "field2" {"field3" ?val2}}))
           {::gf/val1 ["field1"]
            ::gf/val2 ["field2" "field3"]}))))

(deftest extract-list-param
  (testing "extract qualified symbol within a vector"
    (is (= (gf/extract-params (gf/qualify
                                {"field1" ?val1
                                 "field2" [?val2]}))
           {::gf/val1 ["field1"]
            ::gf/val2 ["field2" ::gf/list]}))))

(deftest extract-as-symbol
  (testing "extract qualified symbols from a nested map with ?as"
    (is (= (gf/extract-params (gf/qualify
                                {"field1" ?val1
                                 "field2" {?as ?val2
                                           "field3" ?val3}}))
           {::gf/val1 ["field1"]
            ::gf/val2 ["field2"]
            ::gf/val3 ["field2" "field3"]}))))

(deftest unique-symbol
  (testing (str "extract-params should throw assertion error when"
                " same symbol is used multiple times.")
    (is (thrown? AssertionError
                 (gf/extract-params (gf/qualify
                                      {"field1" ?val1
                                       "field2" ?val1}))))))

(def flat-map-in-format
  (gf/qualify {"field1" ?val1 "field2" ?val2}))
(def flat-map-out-format
  (gf/qualify {:field1 ?val1 :field2 ?val2}))
(def flat-map-convert-fn
  (gf/converter flat-map-in-format flat-map-out-format))
(def flat-map-input-data {"field1" 1 "field2" 2})
(def flat-map-expected-output {:field1 1 :field2 2})
(deftest convert-flat-map
  (testing "convert flat data from flat format to flat format"
    (is (= (flat-map-convert-fn flat-map-input-data)
           flat-map-expected-output))
    (is (= (gf/convert flat-map-in-format flat-map-out-format
                       flat-map-input-data)
           flat-map-expected-output))))

(def nested->flat-in-format
  (gf/qualify {"field1" ?val1 "field2" {"field3" ?val2}}))
(def nested->flat-out-format
  (gf/qualify {?val1 ?val2}))
(def nested->flat-convert-fn
  (gf/converter nested->flat-in-format nested->flat-out-format))
(def nested->flat-input-data {"field1" 1 "field2" {"field3" 2}})
(def nested->flat-expected-output {1 2})
(deftest convert-nested->flat
  (testing "convert nested data to a flat format"
    (is (= (nested->flat-convert-fn nested->flat-input-data)
           nested->flat-expected-output))
    (is (= (gf/convert nested->flat-in-format
                       nested->flat-out-format
                       nested->flat-input-data)
           nested->flat-expected-output))))

(def nested-map-in-format
  (gf/qualify {"field1" ?val1 "field2" {"field3" ?val2}}))
(def nested-map-out-format
  (gf/qualify {:a ?val1 :b {:c ?val2}}))
(def nested-map-convert-fn
  (gf/converter nested-map-in-format nested-map-out-format))
(def nested-map-input-data {"field1" 1 "field2" {"field3" 2}})
(def nested-map-expected-output {:a 1 :b {:c 2}})
(deftest convert-nested-map
  (testing "convert nested data to a nested format"
    (is (= (nested-map-convert-fn nested-map-input-data)
           nested-map-expected-output))
    (is (= (gf/convert nested-map-in-format nested-map-out-format
                       nested-map-input-data)
           nested-map-expected-output))))

; can't test gf/converter because it crashes at compile time
(deftest convert-detect-unbinded-param
  (testing "convert format should contain no unbinded parameter"
    (is (thrown? AssertionError
                 (gf/convert
                   (gf/qualify {"field1" ?v1})
                   (gf/qualify ?v2)
                   {"field1" 1})))))

(deftest convert-list-binding
  (let [in-format (gf/qualify {"field1" ?v1 "field2" [?v2]})
        out-format (gf/qualify [?for [?v2] ?v2])
        data {"field1" 1 "field2" [2 3 4]}
        converter (eval (gf/create-fn-expression
                          in-format out-format))
        expected [2 3 4]]
    (testing "convert to a vector container"
      (is (= (converter data) expected)))))

(deftest convert-list-iterative-multiple-binding
  (let [in-format (gf/qualify {"field1" ?v1 "field2" [?v2]})
        out-format (gf/qualify [?for [?v2] ?v1 ?v2])
        data {"field1" 1 "field2" [2 3 4]}
        converter (eval (gf/create-fn-expression
                          in-format out-format))
        expected [1 2 1 3 1 4]]
    (testing "convert with multiple parameters binded to multiple values"
      (is (= (converter data) expected)))))

(deftest convert-list-iterative-shorthand
  (let [in-format (gf/qualify {"field1" ?v1 "field2" [?v2]})
        out-format (gf/qualify [?for ?v2])
        data {"field1" 1 "field2" [2 3 4]}
        converter (eval (gf/create-fn-expression
                          in-format out-format))
        expected [2 3 4]]
    (testing "convert to vector with shorthand notation"
      (is (= (converter data) expected)))))

(deftest convert-iterative-binding-detect-error
  (testing (str "Assert error when parameters in the same "
                "collection bind to values originally from "
                "different lists")
    (is (thrown? AssertionError
                 (-> {"field1" [?v1]
                        "field2" [?v2]}
                       gf/qualify
                       (gf/convert
                         (gf/qualify [[?for ?v1 ?v2] ?v1 ?v2])
                         {"field1" [1 2 3]
                          "field2" [2 3 4]}))))))

(deftest convert-map-iterative-binding
  (let [in-format (gf/qualify {"entries" [{"name" ?name
                                           "value" ?value}]})
        out-format (gf/qualify {?for [?name ?value]
                                ?name ?value})
        data {"entries" [{"name" "a", "value" 1}
                         {"name" "b", "value" 2}]}
        converter (eval (gf/create-fn-expression
                          in-format out-format))
        expected {"a" 1, "b" 2}]
    (testing "convert with parameters binded to multiple values in map"
      (is (= (converter data) expected)))))

(comment
  (deftest convert-set-iterative-binding
    (testing "bind multiple values to a set"
      (is (= (-> {"field1" ?v1
                "field2" [?v2]}
               gf/qualify
               (gf/convert (gf/qualify #{[?for ?v2] ?v2})
                           {"field1" 1
                            "field2" [2 3 4]}))
           #{2 3 4}))))
)

(deftest convert-nested-list-iterative-binding
  (let [in-format (gf/qualify {"field1" ?v1
                               "field2" [{"list" ?v2
                                          "sublist" [?v3]}]})
        out-format (gf/qualify [?for [?v2]
                                [?v2 [?for [?v3] ?v3]]])
        data {"field1" 1
              "field2" [{"list" "cat1"
                         "sublist" ["subcat1"
                                    "subcat2"]}
                        {"list" "cat2"
                         "sublist" ["subcat1"
                                    "subcat2"]}]}
        converter (eval (gf/create-fn-expression
                          in-format out-format))
        expected [["cat1" ["subcat1" "subcat2"]]
                  ["cat2" ["subcat1" "subcat2"]]]]
    (testing "convert parameter binded to multiple values"
      (is (= (converter data) expected)))))

