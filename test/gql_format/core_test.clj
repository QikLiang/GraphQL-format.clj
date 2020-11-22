(ns gql-format.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [gql-format.core :as gf]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(reset! gf/gen-asserts true)

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
    (is (= (gf/query (gf/qualify {"field1" ?param1 "field2" ?_}))
           "{field1 field2}"))))

(deftest build-full-query
  (testing "build a full query with query variables"
    (is (= (-> {"query" {:$arg1 "Int"
                         "field" {:id "$arg1"
                                  "subfield" ?_}}}
               gf/qualify gf/query)
           "query($arg1: Int){field(id: $arg1){subfield}}"))))

(deftest build-nested-query
  (testing "build a query with nested fields"
    (is (= (gf/query (gf/qualify
                       {"field1" ?param1
                        "field2" ?_
                        "field3" {"nested1" ?n1
                                  "nested2" ?n2}}))
           "{field1 field2 field3{nested1 nested2}}"))))

(deftest build-argument-query
  (testing "build a query with argument fields"
    (is (= (gf/query (gf/qualify
                       {"field1" ?param1
                        "field2" {:arg1 "val1"
                                  :arg2 3
                                  "nested1" ?n1
                                  "nested2" ?n2}}))
           "{field1 field2(arg1: val1, arg2: 3){nested1 nested2}}"))))

(deftest build-list-query
  (testing "build a query containing a list field"
    (is (= (gf/query (gf/qualify
                       {"field1" ?param1
                        "field2" [:arg1 3
                                  :arg2 "hi"
                                  {"field3" ?_}]}))
           "{field1 field2(arg1: 3, arg2: hi){field3}}"))))

(deftest build-as-query
  (testing "build a query with ?as bindings"
    (is (= (gf/query (gf/qualify
                       {?as ?all
                        "field1" [?as ?list
                                  {?as ?list-item
                                   "field2" ?val}]}))
           "{field1{field2}}"))))

(deftest convert-flat-map
  (testing "convert flat data from flat format to flat format"
    (let [in-format (gf/qualify {"field1" ?val1 "field2" ?val2})
          out-format (gf/qualify {:field1 ?val1 :field2 ?val2})
          converter (gf/converter in-format out-format)
          input {"field1" 1 "field2" 2}
          expected {:field1 1 :field2 2}]
      (is (= (converter input) expected)))))

(deftest convert-nested->flat
  (testing "convert nested data to a flat format"
    (let [in-format (gf/qualify {"field1" ?val1
                                 "field2" {"field3" ?val2}})
          out-format (gf/qualify {?val1 ?val2})
          converter (gf/converter in-format out-format)
          input {"field1" 1 "field2" {"field3" 2}}
          expected {1 2}]
      (is (= (converter input) expected)))))

(deftest convert-nested-map
  (testing "convert nested data to a nested format"
    (let [in-format (gf/qualify {"field1" ?val1
                                 "field2" {"field3" ?val2}})
          out-format (gf/qualify {:a ?val1 :b {:c ?val2}})
          converter (gf/converter in-format out-format)
          input {"field1" 1 "field2" {"field3" 2}}
          expected {:a 1 :b {:c 2}}]
      (is (= (converter input) expected)))))

(deftest convert-detect-unbinded-param
  (testing "convert format should contain no unbinded parameter"
    (is (thrown? AssertionError
                 (gf/create-fn-expression
                   (gf/qualify {"field1" ?v1})
                   (gf/qualify ?v2)
                   false)))))

(deftest convert-list-binding
  (let [in-format (gf/qualify {"field1" ?v1 "field2" [?v2]})
        out-format (gf/qualify [?for [?v2] ?v2])
        data {"field1" 1 "field2" [2 3 4]}
        converter (gf/converter in-format out-format)
        expected [2 3 4]]
    (testing "convert to a vector container"
      (is (= (converter data) expected)))))

(deftest convert-list-iterative-multiple-binding
  (let [in-format (gf/qualify {"field1" ?v1 "field2" [?v2]})
        out-format (gf/qualify [?for [?v2] ?v1 ?v2])
        data {"field1" 1 "field2" [2 3 4]}
        converter (gf/converter in-format out-format)
        expected [1 2 1 3 1 4]]
    (testing "convert with multiple parameters binded to multiple values"
      (is (= (converter data) expected)))))

(deftest convert-list-iterative-shorthand
  (let [in-format (gf/qualify {"field1" ?v1 "field2" [?v2]})
        out-format (gf/qualify [?for ?v2])
        data {"field1" 1 "field2" [2 3 4]}
        converter (gf/converter in-format out-format)
        expected [2 3 4]]
    (testing "convert to vector with shorthand notation"
      (is (= (converter data) expected)))))

(comment
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
)

(deftest convert-map-iterative-binding
  (let [in-format (gf/qualify {"entries" [{"name" ?name
                                           "value" ?value}]})
        out-format (gf/qualify {?for [?name ?value]
                                ?name ?value})
        data {"entries" [{"name" "a", "value" 1}
                         {"name" "b", "value" 2}]}
        converter (gf/converter in-format out-format)
        expected {"a" 1, "b" 2}]
    (testing "convert with parameters binded to multiple values in map"
      (is (= (converter data) expected)))))

(deftest convert-set-iterative-binding
  (let [in-format (gf/qualify {"field1" ?v1 "field2" [?v2]})
        out-format (gf/qualify #{?for ?v2})
        data {"field1" 1 "field2" [2 3 4]}
        converter (gf/converter in-format out-format)
        expected #{2 3 4}]
    (testing "bind multiple values to a set"
      (is (= (converter data) expected)))))

(deftest convert-set-iterative-multiple-binding
  (let [in-format (gf/qualify {"field1" ?v1
                               "field2" [{"val1" ?subval1
                                          "val2" ?subval2}]})
        out-format (gf/qualify #{?for ?subval1 ?subval2})
        data {"field1" 1 "field2" [{"val1" "v1a", "val2" "v2a"}
                                   {"val1" "v1b", "val2" "v2b"}
                                   {"val1" "v1c", "val2" "v2c"}]}
        converter (gf/converter in-format out-format)
        expected #{"v1a" "v2a" "v1b" "v2b" "v1c" "v2c"}]
    (testing "bind multiple parameters in a set"
      (is (= (converter data) expected)))))

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
        converter (gf/converter in-format out-format)
        expected [["cat1" ["subcat1" "subcat2"]]
                  ["cat2" ["subcat1" "subcat2"]]]]
    (testing "convert parameter binded to multiple values"
      (is (= (converter data) expected)))))

(deftest converter-with-user-code
  (let [in-format (gf/qualify {"values" [?vals]})
        out-format (gf/qualify `(reduce + ?vals))
        data {"values" [1 2 3 4]}
        converter (gf/converter in-format out-format)
        expected 10]
    (testing "convert parameter binded to multiple values"
      (is (= (converter data) expected)))))

(deftest converter-trigger-assert
  (let [in-format (gf/qualify {"values" [?vals]})
        out-format (gf/qualify `(reduce + ?vals))
        data {"values" 1}
        converter (gf/converter in-format out-format)]
    (testing "Assert should fail when expecting list and not found"
      (is (thrown? AssertionError
                   (converter data))))))
