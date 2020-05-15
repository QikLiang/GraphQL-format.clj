(ns gql-format.core
  (:require [clojure.walk :refer [postwalk]]
            [clojure.string :as str]))

; prefix namespace used to ensure keywords are unique
(def prefix (str (ns-name *ns*)))

(defmacro qualify
  "recursively find all symbols of the form ?param and convert
   them into the form :gql-format.core/param"
  [query]
  (letfn [(convert [sym]
            (as-> sym $
              (name $)
              (subs $ 1)
              (str prefix "/" $)
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
  (and (keyword? value) (= prefix (namespace value))))

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
                                     (dissoc m ::as))]
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
                (= (first vnext) ::as)
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
              (= obj ::as) obj
              (qualified? obj) (list "" "")
              (map-entry? obj) obj
              (vector? obj) (convert-vec obj)
              :else obj))]
    ; only return the fields part of the tuple
    (second (postwalk convert output))))


; transform output to desired format
(defn extract-bindings
  "From a nested map, extract a list of tuples from all qualified
   keywords from the map to a list of the paths to that
   keyword had get-in been used."
  [query]
  ; perform tree traversal where each tree node is a tuple of
  ; the path from the root to the current node and the sub-tree
  (letfn [(val-is-map? [[_ v]] (map? v))
          (val-qualified? [[_ v]] (qualified? v))
          (unwrap-map [[p m]] (for [[k v] m]
                                (if (= k ::as)
                                  [p v]
                                  [(conj p k) v])))
          (unwrap-vec [[p v]] (for [n v]
                                [(conj p ::list) n]))
          (unwrap [tuple] (if (val-is-map? tuple)
                            (unwrap-map tuple)
                            (unwrap-vec tuple)))]
    (filter val-qualified?
      (tree-seq val-is-map? unwrap-map [[] query]))))

(defn last-bindings
  "A version of binding extraction which, when there are multiple
   instances of the same qualified symbol, will choose the last
   occurance in depth-first order."
  [query]
  (reduce (fn [m [p v]] (assoc m v p)) {}
          (extract-bindings query)))

(defn parse-output
  "Intending to be used when binding parameters to query output.
   Extract bindings from a query and asserting that each qualified
   symbol is unique. When there are duplicates of the same symbol,
   it throws."
  [query]
  (reduce (fn [m [p v]]
            (assert (not (contains? m v)))
            (assoc m v p))
          {} (extract-bindings query)))

(defn parse-format
  "Intended to be used when applying bindings to formated output.
  Extract bindings from a query and keep all paths to all symbols."
  [query]
  (extract-bindings query))


; conversion between formats
(defn convert
  "Given the parsed mapping for two formats, convert the data
  from one format to another.
  in   - expected to be produced by parse-output
  out  - expected to be produced by parse-format
  data - expected to conform to the shape of in"
  [in out data]
  ; use list to represent a stack
  (loop [processed (list (list))          ; elements converted
         waiting (list (into (list) out)) ; elements not converted
         colls   (list (empty out))]      ; shape of containers
    (let [[cur-wait           & rest-wait] waiting
          [cur-elem           & rest-elem] cur-wait
          [cur-proc next-proc & rest-proc] processed]
      (cond
        (empty? cur-wait)
        (cond
          ; all elements processed, exit func
          (nil? rest-wait) (into (peek colls) cur-proc)

          (= :map-entry (peek colls))
          (recur (->> cur-proc
                      (conj next-proc)
                      (conj rest-proc))
                 rest-wait
                 (pop colls))

          ; pop layer from stacks
          :else (recur (->> cur-proc
                            (into (peek colls))
                            (conj next-proc)
                            (conj rest-proc))
                       rest-wait
                       (pop colls)))

        (map? (peek colls)) (recur (conj processed [])
                                   ; move first entry in current
                                   ; map into a stack layer
                                   ; of its own
                                   (conj (conj rest-wait
                                               rest-elem)
                                         cur-elem)
                                   (conj colls :map-entry))

        (map? cur-elem) (recur (conj processed (list))
                               (conj (conj rest-wait rest-elem)
                                     (seq cur-elem))
                               (conj colls {}))

        ; convert one element, move it
        ; from waiting to processed, recur
        (qualified? cur-elem)
        (recur (->> (in cur-elem)
                    (get-in data)
                    (conj cur-proc)
                    (conj (pop processed)))
               (conj rest-wait rest-elem)
               colls)

        ; move one element from waiting to processed
        ; without making any conversion
        :else (recur (->> cur-elem
                          (conj cur-proc)
                          (conj (pop processed)))
                     (conj rest-wait rest-elem)
                     colls)))))
