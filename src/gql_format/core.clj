(ns gql-format.core
  (:require [clojure.walk :refer [postwalk]]
            [clojure.string :as str]))

; prefix namespace used to ensure keywords are unique
(def prefix (str (ns-name *ns*)))

(defn qualify-kw
  "Convert a symbol in the form ?param into a keyword of the
   form :gql-format.core/param."
  [obj]
  (if (and (symbol? obj)
           (= \? (first (name obj))))
    (as-> obj $
      (name $)
      (subs $ 1)
      (str prefix "/" $)
      (keyword $))
    obj))

(defmacro qualify
  "Recursively call qualify-kw on all values in a data structure."
  [query]
  (postwalk qualify-kw query))

(defn qualified?
  "check if value matches the format of the output of qualify-kw"
  [value]
  (and (keyword? value) (= prefix (namespace value))))

(defn dequalify-kw
  "The inverse function of qualify-kw"
  [obj]
  (if (qualified? obj)
    (as-> obj $
      (name $)
      (str/split $ #"/")
      (last $)
      (str "?" $)
      (symbol $))
    obj))

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
          (val-qualified? [[_ v]] (and (qualified? v)
                                       (not= v ::_)))
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

(defn- empty-coll [coll]
  (if (map-entry? coll) [] (empty coll)))

(defn- extract-coll-params
  "Detect whether a collection has the form
   [[?for ?v1 ?v2...] shape] and return [(?v1 ?v2 ...) shape].
   Return nil if pattern not match."
  [coll]
  (cond
    (and (sequential? coll)
         (vector? (first coll))
         (= ::for (first (first coll))))
    [(rest (first coll)) (rest coll)]

    (set? coll)
    (let [is-param (group-by #(and (vector? %)
                                   (= ::for (first %)))
                             coll)]
      [(rest (first (is-param true))) (is-param false)])

    :else nil))

(defn- split-list
  "Split a sequence using the keyword ::list. To distinguish
   between [:a :b ::list] and [:a :b], append empty list at the
   end if the original sequence ends with ::list."
  [path]
  (loop [chunks []
         subpath path]
    (let [[path-chunk path-rest]
          (split-with (complement #{::list}) subpath)
          rest-path (rest path-rest)]
      (cond
        (empty? path-rest) (conj chunks path-chunk)
        (empty? rest-path) (conj (conj chunks path-chunk) (list))
        :else (recur (conj chunks path-chunk) rest-path)))))

(defn- extract-list-bindings
  "Given a list of parameters that can bind to multiple values
   and the existing binding, generate a sequence of new bindings
   such that the parameters are binded to each possible
   combination of values."
  [in-format params bindings]
  (loop [[[len to-list post-list :as paths]
          & rest-paths :as all-paths]
         (->> (select-keys in-format params)
              (map #(update % 1 split-list))
              (group-by #(count (second %)))
              (map (fn [[len paths]]
                     (assert (apply = (map #(pop (second %))
                                           paths))
                             (str "Parameters "
                                  (map dequalify-kw params)
                                  " must extract values from the"
                                  " same collection"))
                     [len
                      ; part before the last ::list,
                      ; first instead of map because
                      ; they should all be the same
                      (pop (second (first paths)))
                      ; a mapping from parameters to
                      ; the part after the last ::list
                      (map #(update % 1 peek) paths)]))
              (sort-by first))
         depth (:depth bindings)
         results [bindings]]
    (cond
      (empty? paths) results

      (< (inc depth) len)
      (recur all-paths (inc depth)
             (mapcat (fn [result]
                       (for [new-data (get-in
                                        (:data result)
                                        (to-list depth)
                                        ::unbinded)]
                         (-> result
                             (assoc :data new-data)
                             (update :depth inc))))
                     results))

      :else
      (recur rest-paths depth
             (for [result results]
               (into result
                     (for [[param path] post-list]
                       [param
                        (get-in (:data result) path ::unbinded)])))))))

; conversion between formats
(defn convert
  "Given the parsed mapping for two formats, convert the data
  from one format to another.
  in-format  - expected to be produced by parse-output
  out-format - format of desired output
  data       - expected to conform to the shape of in"
  [in-format out-format data]
  ; Perform in-place binding on all qualified parameters in
  ; out-format by performing a recursive tree traversal through
  ; the data structure. Start with a sequence of the unconverted
  ; values in waiting, take one out, recursively convert it,
  ; then put it at the end of processed. When encountering a
  ; collection, push its contents as a new layer onto the stack
  ; of waiting values, and remember the type of the collection
  ; in colls. When all values in a layer is processed, put the
  ; values back into the same type of collection. Use bindings
  ; to keep track of the cases where a parameter can bind to
  ; multiple values.
  (loop [processed (list (list))
         waiting (list (list out-format))
         colls   (list (list))
         bindings (->> (for [[param path] in-format]
                         [param (get-in data path ::unbinded)])
                       ; :depth is used in extract-list-bindings
                       (into {:data data :depth 0})
                       list list)]
    (let [[cur-wait           & rest-wait] waiting
          [cur-elem           & rest-elem] cur-wait
          [cur-proc next-proc & rest-proc] processed]
      (cond
        ; when all possible bindings are exausted, pop the layer
        ; of bindings, append results at the end of previous
        ; layer's processed
        (empty? (first bindings))
        (recur (->> cur-proc
                    (into (second colls))
                    (conj next-proc)
                    (conj rest-proc))
               (conj (pop (pop rest-wait))
                     (rest (peek (pop rest-wait))))
               (pop (pop colls))
               (pop bindings))

        (empty? cur-wait)
        (cond
          ; all elements processed, exit func
          (nil? rest-wait) (first cur-proc)

          ; pop layer from bindings
          (= :bindings (peek colls))
          (recur processed
                 (conj rest-wait (peek rest-wait))
                 colls
                 (conj (pop bindings) (rest (peek bindings))))

          :else
          ; pop layer from stacks
          (recur (->> cur-proc
                      (into (peek colls))
                      (conj next-proc)
                      (conj rest-proc))
                 rest-wait
                 (pop colls)
                 bindings))

        (coll? cur-elem)
        (let [[coll-params element]
              (extract-coll-params cur-elem)]
        (if (nil? coll-params)
          (recur (conj processed [])
                 (conj (conj rest-wait rest-elem)
                       (seq cur-elem))
                 (conj colls (empty-coll cur-elem))
                 bindings)
          (recur (conj processed [])
                 ; keep cur-elem unprocessed so it
                 ; can be reused for multiple bindings
                 (conj (conj waiting element) element)
                 (conj (conj colls (empty cur-elem)) :bindings)
                 (conj bindings (extract-list-bindings
                                  in-format
                                  coll-params
                                  (first (first bindings)))))))

        ; move one element from waiting to processed
        ; bind value if element is qualified param
        :else (recur (->> (if (qualified? cur-elem)
                            (let [binded
                                  (get (first (first bindings))
                                       cur-elem
                                       ::unbinded)]
                              (assert (not= binded ::unbinded)
                                      (str "Parameter "
                                           (dequalify-kw cur-elem)
                                           "unresolved"))
                              binded)
                            cur-elem)
                          (conj cur-proc)
                          (conj (pop processed)))
                     (conj rest-wait rest-elem)
                     colls
                     bindings)))))
