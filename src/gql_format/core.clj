(ns gql-format.core
  (:require [clojure.walk :refer [walk postwalk prewalk]]
            [clojure.string :as str]))

; prefix namespace used to ensure keywords are unique
(def prefix (str (ns-name *ns*)))

(defn- qualify-kw
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

(defn- qualified?
  "check if value matches the format of the output of qualify-kw"
  [value]
  (and (keyword? value) (= prefix (namespace value))))

(defn- dequalify-kw
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

(defn quote
  "Since both GraphQL and Clojure use double-quotes for strings,
   this helper function surrounds text with double-quotes."
  [text]
  (str \" text \"))

(defn- build-query-fields
  "take a mapping of fields to converted text and
   join them to form a block"
  [fields]
  (str \{
       (str/join " " (for [[k v] fields]
                       (str k (first v) (second v))))
       \}))

(defn- build-query-params
  "take a list of key value pairs of GraphQL
   field arguments and join them"
  [params]
  (if (empty? params) ""
    (str \(
         (str/join ", "
                   (for [[k v] params
                         :when (not (qualified? k))]
                     (str (name k) ": " v)))
         \))))

(defn- build-query-map
  "separate a map into arguments and fields,
   convert them separately, and return a tuple"
  [m]
  (let [keywords (group-by #(keyword? (first %))
                           (dissoc m ::as))]
    (list
      (build-query-params (keywords true))
      (build-query-fields (keywords false)))))

(defn- build-query-vec
  "treat the first 2N elements such that the even
   indices are keywords as argument key value pairs,
   return a tuple of the converted arguments and
   the 2N+1th element. Assume 2N+1th element is
   output from convert with no arguments"
  [v]
  (loop [vnext v
         args []]
    (cond
      (= (first vnext) ::as)
      (recur (drop 2 vnext) args)

      (keyword? (first vnext))
      (recur (drop 2 vnext) (conj args (take 2 vnext)))

      :else
      (list (build-query-params args) (second (first vnext))))))

(defn- build-subquery
  "dispatch to the proper build-query-* based on type"
  [obj]
  (cond
    (map? obj) (build-query-map obj)
    (= obj ::as) obj
    (qualified? obj) (list "" "")
    (map-entry? obj) obj
    (vector? obj) (build-query-vec obj)
    :else obj))

(defn build
  "build a GraphQL query string using the expected output shape"
  [output]
  (if (and (map? output) (contains? output "query"))
    (str "query" (str/join "" (postwalk build-subquery
                                        (output "query"))))
    (second (postwalk build-subquery output))))


; transform output to desired format
(defn- extract-coll-params
  "Detect whether a collection has the form
   [[?for ?v1 ?v2...] shape] and return [(?v1 ?v2 ...) shape].
   Return nil if pattern not match."
  [coll]
  (cond
    (and (sequential? coll)
         (vector? (first coll))
         (= ::for (first (first coll))))
    [(rest (first coll)) (into (empty coll) (rest coll))]

    (and (map? coll)
         (contains? coll ::for))
    [(::for coll) (dissoc coll ::for)]

    (set? coll)
    (let [is-param (group-by #(and (vector? %)
                                   (= ::for (first %)))
                             coll)]
      [(rest (first (is-param true)))
       (apply disj (is-param false))])

    :else nil))

(defn extract-bindings
  "From a nested map, extract a list of tuples from all qualified
   keywords from the map to a list of the paths to that
   keyword had get-in been used."
  [query rep-coll]
  ; perform tree traversal where each tree node is a tuple of
  ; the path from the root to the current node and the sub-tree
  (letfn [(val-is-coll? [[_ v]] (coll? v))
          (val-qualified? [[_ v]] (and (qualified? v)
                                       (not= v ::_)))
          (unwrap-map [[p m]] (mapcat (fn [[k v]]
                                        (if (= k ::as)
                                          [[p v]]
                                          [[(conj p k) v]
                                           [(conj p ::key) k]]))
                                      m))
          (unwrap-seq [[p s]] (if rep-coll
                                (map (fn [v]
                                       [(conj p ::list) v])
                                     s)
                                (map-indexed (fn [idx v]
                                               [(conj p idx) v])
                                             s)))
          (unwrap-simple [[_ v :as tuple]]
            (cond (map? v)
                  (unwrap-map tuple)
                  (sequential? v)
                  (unwrap-seq tuple)))
          (unwrap [[path value :as tuple]]
            (let [coll-params (extract-coll-params value)]
              (if (nil? coll-params)
                (unwrap-simple tuple)
                (list [(conj path coll-params) ::for]))))]
    (filter val-qualified?
      (tree-seq val-is-coll? unwrap [[] query]))))

(defn extract-params
  "Intending to be used when binding parameters to query output.
   Extract bindings from a query and asserting that each qualified
   symbol is unique. When there are duplicates of the same symbol,
   it throws."
  [query]
  (reduce (fn [m [p v]]
            (assert (not (contains? m v)))
            (assoc m v p))
          {} (extract-bindings query true)))

(defn parse-format
  [query]
  (update
    (reduce (fn [m [p v]] (assoc m v (conj (get m v []) p)))
            {} (extract-bindings query false))
    ::for
    (partial mapv #(update % (- (count %) 1)
                           (fn [[params subquery]]
                             [params subquery
                              (parse-format subquery)])))))


; conversion between formats
(defn- empty-coll [coll]
  (if (map-entry? coll) [] (empty coll)))

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
  [params-loc params bindings]
  (loop [[[len to-list post-list :as paths]
          & rest-paths :as all-paths]
         (->> (select-keys params-loc params)
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

(defn- form-binding
  "construct the initial binding used by convert"
  [params-loc data]
  (->> (for [[param path] params-loc]
         [param (get-in data path ::unbinded)])
       ; :depth is used in extract-list-bindings
       (into {:data data :depth 0})))

(defn- assoc-on
  "assoc-in handling the edge case where path is []"
  [m p v]
  (if (empty? p) v (assoc-in m p v)))

(defn- update-on
  "assoc-in handling the edge case where path is []"
  [m p f]
  (if (empty? p) (f m) (update-in m p f)))

(defn reshape
  "An optimized version of convert when ?for is not used."
  [shape params bindings]
  (reduce (fn [shape [param paths]]
            (loop [new-shape shape
                   [path rest-paths] paths]
              (assert (contains? bindings param)
                  (str "Symbol " (dequalify-kw param)
                       " not found in input format"))
              (cond
                (nil? path) new-shape
                (= ::key (peek path))
                (update-on new-shape (pop path)
                           (fn [sub-shape]
                             (assoc (dissoc sub-shape param)
                                    (bindings param)
                                    (sub-shape param))))
                :else (recur (assoc-in new-shape path
                                       (bindings param))
                             rest-paths))))
          shape params))

(defn apply-binding
  [in-params out-params out-format binding_]
  (loop [result (reshape out-format (dissoc out-params ::for)
                          binding_)
         for-list (::for out-params)]
    (if (empty? for-list)
      result
      (let [cur-list (peek for-list)
            cur-path (pop cur-list)
            [coll-params sub-format sub-params] (peek cur-list)]
        (assoc-on result cur-path
                  (into (empty sub-format)
                        (mapcat #(apply-binding in-params
                                                sub-params
                                                sub-format %))
                        (extract-list-bindings in-params
                                               coll-params
                                               binding_)))))))

(defn convert
  "Given the parsed mapping for two formats, convert the data
  from one format to another.
  params-loc - expected to be produced by extract-params
  out-format - format of desired output
  data       - expected to conform to the shape of in"
  ([in-format out-format data]
   (convert (extract-params in-format)
                  (parse-format out-format) out-format
                  data))
  ([in-params out-params out-format data]
   (apply-binding in-params out-params out-format
                  (form-binding in-params data))))

(defmacro precompile
  "Evaluate query building and parameters extraction at compile time.
  Return [(build (qualify query))
  (extract-params (qualify query))]."
  [query]
  (let [q (qualify query)]
    [(build q) (extract-params q)]))

(def d2
  (into {} (for [i (range 100)]
             [i (into {} (for [j (range 100)]
                           [j (str i "." j)]))])))

(def inf (into {} (for [i (range 50 70)]
                    [i (into {} (for [j (range 50 70)]
                                  [j (keyword (str prefix "/" i "." j))]))])))

(def outf (into {} (for [i (range 50 70)
                         j (range 50 70)]
                     [(str i "." j)
                      (keyword (str prefix "/" i "." j))])))
(def ip (extract-params inf))
(def op (parse-format outf))
(def b (form-binding ip d2))

(def data
  {"form"
   (into [] (for [field (range 10)]
              {"field" field
               "subfields"
               (into [] (for [subfield (range 10)]
                          {"name" (str subfield)
                           "value" subfield}))}))})
(def params (-> {"form" [{"field" ?field
                          "subfields" [{"name" ?name
                                        "value" ?value}]}]}
                qualify extract-params))
(def output-format (qualify [[?for ?field ?name ?value]
                             {:field ?field
                              :name ?name
                              :value ?value}]))
(def out-params (parse-format output-format))

(def simp-param (qualify
                  {"entries" [{"name" ?name "value" ?value}]}))
(def simp-data {"entries" [{"name" "a", "value" 1}
                           {"name" "b", "value" 2}]})
(def simp-output (qualify {?for [?name ?value]
                           ?name ?value}))

(defn prewalk-inherit
  "Like prewalk, but allow an extra state to be passed from parent
   to children. Expect f to take parameters [form & state] and
   return [[children] & new-state] where new-state must be
   acceptable by a new call to f."
  [f form state]
  (let [[children & new-state] (f form state)]
    (walk #(apply prewalk-inherit f % new-state)
          identity children)))

(defn iter-var [iter]
  (if (zero? iter)
    '?data
    (symbol (str "iter-var-" iter))))

(defn iter-depth [path]
  (count (filter #{(qualify ?list)} path)))

(defn create-get-in-exp [path]
  (let [iter-paths (split-list path)
        iter (dec (count iter-paths))
        get-path (last iter-paths)
        var-symbol (iter-var iter)]
    (if (empty? get-path)
      var-symbol
      `(get-in ~var-symbol
               [~@(last (split-list path))]))))

(declare create-inner-form)

(defn create-let-expression
  [form get-symbol param-paths min-iter-depth max-iter-depth]
  `(let
     [~@(apply concat
               (for [[sym path] param-paths
                     :let [iter-len (iter-depth path)]
                     :when (and (<= min-iter-depth iter-len)
                                (<= iter-len max-iter-depth))]
                 [(get-symbol sym) (create-get-in-exp path)]))]
     ~(create-inner-form
        form get-symbol param-paths max-iter-depth)))

(defn create-iter-params
  [iter-params param-paths cur-iter-depth]
  (let [iter-paths (mapv (comp split-list param-paths)
                         iter-params)
        iter-path (apply max-key count iter-paths)
        new-iter-depth (dec (count iter-path))]
    [(vec (mapcat
            (fn [iter]
              [(iter-var (inc iter))
               `(get-in ~(iter-var iter)
                        [~@(nth iter-path iter)])])
            (range cur-iter-depth
                   new-iter-depth)))
     new-iter-depth]))

(defn create-for-expression
  [iter-params sub-form get-symbol param-paths cur-iter-depth]
  (let [[iter-list new-iter-depth]
        (create-iter-params iter-params param-paths cur-iter-depth)]
    `(for ~iter-list
       ~(create-let-expression
          sub-form get-symbol param-paths
          (inc cur-iter-depth) new-iter-depth))))

(defn create-inner-form
  [form get-symbol param-paths cur-iter-depth]
  (assert (or (not (qualified? form))
              (contains? get-symbol form))
          (str "Symbol " (dequalify-kw form)
               " not found in input format"))
  (cond
    (qualified? form) (get-symbol form)

    ; [?for ?param] expressions
    (and (vector? form)
         (= (qualify ?for) (first form))
         (= (count form) 2))
    (let [list-params [(second form)]
          sub-form (second form)
          for-exp (create-for-expression list-params sub-form
                                         get-symbol param-paths
                                         cur-iter-depth)]
      `(vec ~for-exp))

    ; [?for [?params] sub-form] expressions
    (and (vector? form)
         (= (qualify ?for) (first form))
         (= (count form) 3))
    (let [list-params (second form)
          sub-form (last form)
          for-exp (create-for-expression list-params sub-form
                                         get-symbol param-paths
                                         cur-iter-depth)]
      `(vec ~for-exp))

    ; [?for [?params] sub-form1 sub-form2 ...] expressions
    (and (vector? form)
         (= (qualify ?for) (first form))
         (> (count form) 3))
    (let [list-params (second form)
          sub-form (vec (drop 2 form))
          for-exp (create-for-expression list-params sub-form
                                         get-symbol param-paths
                                         cur-iter-depth)]
      `(vec (apply concat ~for-exp)))

    ; {?for [?params] ?key ?val} expressions
    (and (map? form)
         (contains? form (qualify ?for)))
    (let [list-params (form (qualify ?for))
          sub-form (vec (dissoc form (qualify ?for)))
          for-exp (create-for-expression list-params sub-form
                                         get-symbol param-paths
                                         cur-iter-depth)]
      `(into {} (apply concat ~for-exp)))

    ; recursively traverse ordinary data structures
    :else (walk #(create-inner-form
                   % get-symbol param-paths cur-iter-depth)
                identity form)))

(defn create-fn-expression [in-shape out-shape]
  (let [param-paths (extract-params in-shape)
        get-symbol (reduce-kv (fn [m k _]
                                (->> k
                                     ; strip namespace
                                     dequalify-kw
                                     ; generate unique symbol
                                     gensym
                                     (assoc m k)))
                              {}
                              param-paths)]
    (list 'fn '[?data]
       (create-let-expression out-shape get-symbol param-paths 0 0))))

(defmacro converter [in-shape out-shape]
  (create-fn-expression (eval in-shape) (eval out-shape)))

