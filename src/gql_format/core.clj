(ns gql-format.core
  (:require [clojure.walk :refer [walk postwalk prewalk]]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

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

(defn query
  "build a GraphQL query string using the expected output shape"
  [output]
  (if (and (map? output) (contains? output "query"))
    (str "query" (str/join "" (postwalk build-subquery
                                        (output "query"))))
    (second (postwalk build-subquery output))))


; transform output to desired format
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
          (unwrap [[_ v :as tuple]]
            (cond (map? v)
                  (unwrap-map tuple)
                  (sequential? v)
                  (unwrap-seq tuple)))]
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

(defn iter-depth [path]
  (count (filter #{::list} path)))

(defn iter-var [iter]
  (cond
    (seqable? iter) (iter-var (iter-depth iter))
    (zero? iter) '?data
    :else (symbol (str "iter-var-" iter))))

(defn create-get-in-exp [source path]
  (cond
    (empty? path) source
    (= 1 (count path)) `(get ~source ~(first path))
    :else `(get-in ~source [~@path])))

(declare create-inner-form)

(defn create-let-expression
  [form get-symbol param-paths min-iter-depth max-iter-depth]
  (let [[inner-form params-used]
        (create-inner-form
          form get-symbol param-paths max-iter-depth)
        new-params
        (apply concat
               (for [sym params-used
                     :let [path (param-paths sym)
                           iter-paths (split-list path)
                           iter-len (dec (count iter-paths))]
                     :when (and (seq (last iter-paths))
                                (<= min-iter-depth iter-len)
                                (<= iter-len max-iter-depth))]
                 [(get-symbol sym)
                  (create-get-in-exp (iter-var iter-len)
                                     (last iter-paths))]))
        result-form (if (empty? new-params) inner-form
                      `(let [~@new-params] ~inner-form))]
    [result-form params-used]))

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
        (create-iter-params iter-params param-paths cur-iter-depth)
        [let-exp params-used]
        (create-let-expression
          sub-form get-symbol param-paths
          (inc cur-iter-depth) new-iter-depth)
        new-params (into (set params-used)
                         (take-nth 2 iter-list))
        result-form `(for ~iter-list ~let-exp)]
    [result-form new-params]))

(defn create-inner-form
  [form get-symbol param-paths cur-iter-depth]
  (assert (or (not (qualified? form))
              (contains? get-symbol form))
          (str "Symbol " (dequalify-kw form)
               " not found in input format"))
  (cond
    (qualified? form) [(get-symbol form) [form]]

    ; [?for ?param] expressions
    (and (vector? form)
         (= (qualify ?for) (first form))
         (= (count form) 2))
    (let [list-params [(second form)]
          sub-form (second form)
          [for-exp params-used]
          (create-for-expression list-params sub-form
                                 get-symbol param-paths
                                 cur-iter-depth)]
      [`(vec ~for-exp) params-used])

    ; [?for [?params] sub-form] expressions
    (and (vector? form)
         (= (qualify ?for) (first form))
         (= (count form) 3))
    (let [list-params (second form)
          sub-form (last form)
          [for-exp params-used]
          (create-for-expression list-params sub-form
                                 get-symbol param-paths
                                 cur-iter-depth)]
      [`(vec ~for-exp) params-used])

    ; [?for [?params] sub-form1 sub-form2 ...] expressions
    (and (vector? form)
         (= (qualify ?for) (first form))
         (> (count form) 3))
    (let [list-params (second form)
          sub-form (vec (drop 2 form))
          [for-exp params-used]
          (create-for-expression list-params sub-form
                                 get-symbol param-paths
                                 cur-iter-depth)]
      [`(vec (apply concat ~for-exp)) params-used])

    ; #{?for ?param} expressions
    (and (set? form)
         (contains? form (qualify ?for))
         (= (count form) 2))
    (let [list-params (disj form (qualify ?for))
          sub-form (first list-params)
          [for-exp params-used]
          (create-for-expression list-params sub-form
                                 get-symbol param-paths
                                 cur-iter-depth)]
      [`(set ~for-exp) params-used])

    ; #{?for ?param1 ?param2 ...} expressions
    (and (set? form)
         (contains? form (qualify ?for))
         (> (count form) 2))
    (let [list-params (disj form (qualify ?for))
          sub-form (vec list-params)
          [for-exp params-used]
          (create-for-expression list-params sub-form
                                 get-symbol param-paths
                                 cur-iter-depth)]
      [`(set (apply concat ~for-exp)) params-used])

    ; {?for [?params] ?key ?val} expressions
    (and (map? form)
         (contains? form (qualify ?for)))
    (let [list-params (form (qualify ?for))
          sub-form (vec (dissoc form (qualify ?for)))
          [for-exp params-used]
          (create-for-expression list-params sub-form
                                 get-symbol param-paths
                                 cur-iter-depth)]
      [`(into {} (apply concat ~for-exp)) params-used])

    ; recursively traverse ordinary data structures
    :else
    (let [recursive-result
          (walk #(create-inner-form
                   % get-symbol param-paths cur-iter-depth)
                identity form)
          result (walk first identity recursive-result)
          params (if (seqable? recursive-result)
                   (set (mapcat second recursive-result))
                   #{})]
      [result params])))

(defn make-symbol [sym path]
  (let [iter-paths (split-list path)]
    (if (empty? (last iter-paths))
      (iter-var (dec (count iter-paths)))
      (gensym (dequalify-kw sym)))))

(defn create-fn-expression [in-shape out-shape]
  (let [param-paths (extract-params in-shape)
        ; in-line parameters if they're just alias for another
        get-symbol (reduce-kv (fn [m sym path]
                                (assoc m sym
                                       (make-symbol sym path)))
                              {}
                              param-paths)
        [let-exp _]
        (create-let-expression out-shape get-symbol param-paths 0 0)]
    (pprint let-exp)
    (list 'fn '[?data] let-exp)))

(defmacro precompile [in-shape out-shape]
  (create-fn-expression (eval in-shape) (eval out-shape)))
