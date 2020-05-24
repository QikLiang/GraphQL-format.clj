(ns gql-format.performance-test
  (:require [clojure.test :refer [deftest is]]
            [gql-format.core :as gf]
            [criterium.core :refer [bench quick-bench]]))

(def data
  {"form"
   (into [] (for [field (range 10)]
              {"field" field
               "subfields"
               (into [] (for [subfield (range 10)]
                          {"name" (str subfield)
                           "value" subfield}))}))})
(def input-params
  (-> {"form" [{"field" ?field
                "subfields" [{"name" ?name
                              "value" ?value}]}]}
      gf/qualify gf/extract-params))
(def output-format
  (gf/qualify [[?for ?field ?name ?value]
               {:field ?field
                :name ?name
                :value ?value}]))
(def output-params (gf/parse-format output-format))

(defn convert []
  (gf/convert input-params output-params output-format data))
(defn handwriten []
  (into []
          (mapcat (fn [{field "field"
                        subfields "subfields"}]
                    (for [{n "name" v "value"} subfields]
                      {:field field :name n :value v})))
          (data "form")))

(deftest performance
  (is (= (convert) (handwriten)))
  (println "benchmarking gql-format.core/convert")
  (quick-bench (convert))
  (println "benchmarking handwriten conversion")
  (quick-bench (handwriten)))
