(ns gql-format.core-test
  (:require [clojure.test :refer :all]
            [gql-format.core :as gf]))

(deftest simple-qualifier
  (testing "qualify a simgle symbol"
    (is (= :gqlf/symbol (gf/qualify ?symbol)))))

(deftest flat-map-qualifier
  (testing "qualify a flat map"
    (is (= {:key :gqlf/value} (gf/qualify {:key ?value})))))

(deftest nested-map-qualifier
  (testing "qualify a nested map"
    (is (= {:k2 {:key :gqlf/value} :unrelated :value}
           (gf/qualify {:k2 {:key ?value} :unrelated :value})))))

(deftest multiple-qualifiers
  (testing "qualify multiple symbols in a map"
    (is (= {:k1 :gqlf/v1 :k2 :gqlf/v2}
           (gf/qualify {:k1 ?v1 :k2 ?v2})))))
