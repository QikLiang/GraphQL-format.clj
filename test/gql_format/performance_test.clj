(ns gql-format.performance-test
  (:require [clojure.test :refer [deftest is]]
            [gql-format.core :as gf]
            [criterium.core :refer [bench quick-bench]]))

(def data
  {"form"
   (into [] (for [field (range 50)]
              {"field" field
               "subfields"
               (into [] (for [subfield (range 50)]
                          {"name" (str subfield)
                           "value" subfield}))}))})
(def input-format
  (gf/qualify {"form" [{"field" ?field
                        "subfields" [{"name" ?name
                                      "value" ?value}]}]}))
(def output-format
  (gf/qualify [?for [?field ?name ?value]
               {:field ?field
                :name ?name
                :value ?value}]))

(def converter (gf/precompile input-format output-format))

; prevent potential compile-time in-lining
(def t1 (new java.util.Date))
(Thread/sleep 1000)
(def t2 (new java.util.Date))
(def evaluator
  (let [in (if (.before t1 t2) input-format nil)
        out output-format]
    (eval (gf/create-fn-expression in out))))

(defn convert [] (converter data))
(defn evaluate [] (evaluator data))

(defn handwriten []
  (into []
          (mapcat (fn [{field "field"
                        subfields "subfields"}]
                    (for [{n "name" v "value"} subfields]
                      {:field field :name n :value v})))
          (data "form")))

(deftest performance
  (is (= (convert) (handwriten)))
  (println "benchmarking compile-time generated converter")
  (quick-bench (convert))
  (println "benchmarking run-time generated converter")
  (quick-bench (evaluate))
  (println "benchmarking handwriten conversion")
  (quick-bench (handwriten)))
