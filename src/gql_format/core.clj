(ns gql-format.core
  (:require [clojure.walk :refer [postwalk]]
            [clojure.string :as str]))

; prefix namespace used to ensure keywords are unique
(def prefix "gqlf/")

(defmacro qualify
  "recursively find all symbols of the form ?param and convert
   them into the form :gqlf/param"
  [query]
  (letfn [(convert [sym]
            (as-> sym $
              (name $)
              (subs $ 1)
              (str prefix $)
              (keyword $)))
          (check-convert [obj]
            (if (and (symbol? obj)
                     (= \? (first (name obj))))
              (convert obj)
              obj))]
    (postwalk check-convert query)))

(defn qualified?
  "check if value matches the format of the output of qualify"
  [value]
  (and (keyword? value) (= "gqlf" (namespace value))))

(defn build
  "build a GraphQL query string using the expected output shape"
  [output]
  (letfn [(convert-fields [fields]
            ; take a mapping of fields to converted text and
            ; join them to form a block
            (str "{"
                 (str/join " " (for [[k v] fields]
                                 (str k (first v) (second v))))
                 "}"))
          (convert-params [ps]
            ; take a list of key value pairs of GraphQL
            ; field arguments and join them
            (if (empty? ps) ""
              (str "("
                   (str/join ", "
                             (for [[k v] ps
                                   :when (not (qualified? k))]
                               (str (name k) ": " v)))
                   ")")))
          (convert-map [m]
            ; separate a map into arguments and fields,
            ; convert them separately, and return a tuple
            (let [keywords (group-by #(keyword? (first %))
                                     (dissoc m :gqlf/as))]
              (list
                (convert-params (keywords true))
                (convert-fields (keywords false)))))
          (convert-vec [v]
            ; treat the first 2N elements such that the even
            ; indices are keywords as argument key value pairs,
            ; return a tuple of the converted arguments and
            ; the 2N+1th element. Assume 2N+1th element is
            ; output from convert with no arguments
            (loop [vnext v
                   args []]
              (cond
                (= (first vnext) :gqlf/as)
                (recur (drop 2 vnext) args)

                (keyword? (first vnext))
                (recur (drop 2 vnext) (conj args (take 2 vnext)))

                :else
                (list (convert-params args) (second (first vnext))))))
          ; treat map entries differently than normal vectors
          (map-entry? [obj]
            (and (vector? obj) (= 2 (count obj))))
          ; dispatch to the proper convertor based on data shape
          (convert [obj]
            (cond
              (map? obj) (convert-map obj)
              (= obj :gqlf/as) obj
              (qualified? obj) (list "" "")
              (map-entry? obj) obj
              (vector? obj) (convert-vec obj)
              :else obj))]
    ; only return the fields part of the tuple
    (second (postwalk convert output))))
