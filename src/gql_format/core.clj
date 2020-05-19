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
          (val-is-vec? [[_ v]] (vector? v))
          (val-is-coll? [[_ v]] (coll? v))
          (val-qualified? [[_ v]] (qualified? v))
          (unwrap-map [[p m]] (for [[k v] m]
                                (if (= k ::as)
                                  [p v]
                                  [(conj p k) v])))
          (unwrap-vec [[p v]] (for [n v]
                                [(conj p ::list) n]))
          (unwrap [tuple] (cond (val-is-map? tuple)
                            (unwrap-map tuple)
                            (val-is-vec? tuple)
                            (unwrap-vec tuple)))]
    (filter val-qualified?
      (tree-seq val-is-coll? unwrap [[] query]))))

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

(defn- split-list [path]
  (loop [chunks []
         subpath path]
    (let [[path-chunk path-rest]
          (split-with (complement #{::list}) subpath)
          rest-path (rest path-rest)]
      (cond
        (empty? path-rest) (conj chunks path-chunk)
        (empty? rest-path) (conj (conj chunks path-chunk) (list))
        :else (recur (conj chunks path-chunk) rest-path)))))

(defn- bind-list-params
  [in-format params bindings init-depth]
  (loop [[[len to-list post-list :as paths]
          & rest-paths :as all-paths]
         (->> (select-keys in-format params)
              (map #(update % 1 split-list))
              (group-by #(count (second %)))
              (map (fn [[len paths]]
                     [len
                      ; part before the last ::list
                      ; first instead of map because
                      ; they should all be the same
                      (pop (second (first paths)))
                      ; part after the last ::list
                      (map #(update % 1 peek) paths)]))
              (sort-by first))
         depth init-depth
         results [bindings]]
    (cond
      (empty? paths) [depth results]

      (< (inc depth) len)
      (recur all-paths (inc depth)
             (mapcat (fn [result]
                       (for [new-data (get-in
                                        (:data result)
                                        (to-list depth))]
                         (assoc result :data new-data)))
                     results))

      :else
      (recur rest-paths depth
             (for [result results]
               (into result
                     (for [[param path] post-list]
                       [param
                        (get-in (:data result) path)])))))))

; conversion between formats
(defn convert
  "Given the parsed mapping for two formats, convert the data
  from one format to another.
  in-format  - expected to be produced by parse-output
  out-format - format of desired output
  data       - expected to conform to the shape of in"
  [in-format out-format data]
  ; use list to represent a stack
  (loop [processed (list (list))          ; elements converted
         waiting (list (list out-format)) ; elements not converted
         colls   (list (list))            ; shape of containers
         bindings (->> (for [[param path] in-format]
                         [param (get-in data path)])
                       (into {:data data})
                       list list)
         list-iter-depth 0]
    (let [[cur-wait           & rest-wait] waiting
          [cur-elem           & rest-elem] cur-wait
          [cur-proc next-proc & rest-proc] processed]
      (cond
        (empty? (first bindings))
        (recur (->> cur-proc
                    (into (second colls))
                    (conj next-proc)
                    (conj rest-proc))
               (conj (pop rest-wait)
                     (rest (peek rest-wait)))
               (pop (pop colls))
               (pop bindings)
               list-iter-depth)

        (empty? cur-wait)
        (cond
          ; all elements processed, exit func
          (nil? rest-wait) (first cur-proc)

          (= :map-entry (peek colls))
          (recur (->> cur-proc
                      (conj next-proc)
                      (conj rest-proc))
                 rest-wait
                 (pop colls)
                 bindings
                 list-iter-depth)

          (= :bindings (peek colls))
          (recur processed
                 (conj rest-wait
                       (list (nth (first (peek rest-wait)) 2)))
                 colls
                 (conj (pop bindings) (rest (peek bindings)))
                 list-iter-depth)

          :else
          ; pop layer from stacks
          (recur (->> cur-proc
                      (into (peek colls))
                      (conj next-proc)
                      (conj rest-proc))
                 rest-wait
                 (pop colls)
                 bindings
                 list-iter-depth))

        (map? (peek colls))
        ; move first entry in current
        ; map into a stack layer of its own
        (recur (conj processed [])
               (conj (conj rest-wait rest-elem) cur-elem)
               (conj colls :map-entry)
               bindings
               list-iter-depth)

        (map? cur-elem)
        ; move map entries into its own stack layer
        (recur (conj processed (list))
               (conj (conj rest-wait rest-elem) (seq cur-elem))
               (conj colls {})
               bindings
               list-iter-depth)

        (sequential? cur-elem)
        (if (= ::for (first cur-elem))
          (let [[new-depth new-bindings]
                (bind-list-params in-format (second cur-elem)
                                  (first (first bindings))
                                  list-iter-depth)]
            (recur (conj processed [])
                   ; keep cur-elem unprocessed so it can be reused
                   (conj waiting (list (nth cur-elem 2)))
                   (conj (conj colls (empty cur-elem)) :bindings)
                   (conj bindings new-bindings)
                   new-depth))
          (recur (conj processed (list))
                 (conj (conj rest-wait rest-elem) cur-elem)
                 (conj colls (empty cur-elem))
                 bindings
                 list-iter-depth))

        ; convert one element, move it
        ; from waiting to processed, recur
        (qualified? cur-elem)
        (recur (->> ((first (first bindings)) cur-elem)
                    (conj cur-proc)
                    (conj (pop processed)))
               (conj rest-wait rest-elem)
               colls
               bindings
               list-iter-depth)

        ; move one element from waiting to processed
        ; without making any conversion
        :else (recur (->> cur-elem
                          (conj cur-proc)
                          (conj (pop processed)))
                     (conj rest-wait rest-elem)
                     colls
                     bindings
                     list-iter-depth)))))
