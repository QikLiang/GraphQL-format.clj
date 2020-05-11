(ns gql-format.core
  (:require [clojure.walk :refer [postwalk]]))

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
