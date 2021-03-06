(ns gql-format.core
  (:require [clojure.walk :refer [walk postwalk]]
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

(defn query
  "build a GraphQL query string using the expected output shape"
  ([params form]
   (query {"query" (into form params)}))
  ([output]
   (if (and (map? output) (contains? output "query"))
     (str "query" (str/join "" (postwalk build-subquery
                                         (output "query"))))
     (second (postwalk build-subquery output)))))


; transform output to desired format
(defn extract-bindings
  "Recursively find all the qualified symbols in a form."
  [form]
  (cond
    (seqable? form) (set (mapcat extract-bindings form))

    (and (qualified? form)
         (not (contains? #{::for ::for-lazy} form)))
    #{form}

    :else #{}))

(defn extract-dependencies
  "Returns a tuple of two lists of maps with these parameters:
   :param - the key for keeping track of nodes internally
   :symbol - representation in the generated code
   :parent - most recent ancester node in the syntax tree
   :path - current node = (get-in parent path)
   :list - true if current node holds a vector
   :depth - distance from root form
   Paths are constructed as lists so parent nodes can be in-lined
   by conj'ing to the child's path"
  [form parent edge params-used depth]
  (cond
    (= ::as edge) [[] []]

    (map? form)
    (let [self-sym (cond (contains? form ::as)
                     (gensym (str (dequalify-kw (form ::as)) \-))
                     (= edge ::list) (gensym "??iter-var-")
                     :else (gensym (str "??" edge \-)))
          self-param (if (contains? form ::as)
                       (form ::as)
                       (qualify-kw self-sym))
          sub-results (mapv (fn [[k v]]
                              (extract-dependencies
                                v self-param k params-used
                                (inc depth)))
                           form)
          children (mapcat first sub-results)
          other-dependents (mapcat second sub-results)]
      (cond
        (and (not (contains? form ::as)) ; no binding to self
             (empty? children)) ; and no dependents
        [[] []] ; return empty
        ; in-line current node if only used by one child
        (and (= 1 (count children))
             (some? parent) ; don't in-line root
             (not= edge ::list))
        [(for [child children
               :let [path (conj (:path child) edge)]]
           (assoc child :path path :parent parent))
         other-dependents]
        ; otherwise, self as only child and merge dependents
        :else [[{:param self-param
                 :symbol self-sym
                 :path (list edge)
                 :parent parent
                 :depth depth}]
               (concat children other-dependents)]))

    ; current form is an array of values
    (vector? form)
    (let [[sub-form as-param]
          (loop [as-param nil [form1 form2 & form-rest] form]
            (cond
              (= form1 ::as) (recur form2 form-rest)
              (and (keyword? form1) (not (qualified? form1)))
              (recur as-param form-rest)
              :else [form1 as-param]))
          [[{:keys [param symbol]}] other-dependents]
          (extract-dependencies
            (if (and (map? sub-form) (some? as-param))
              (assoc sub-form ::as as-param)
              sub-form)
            nil ::list params-used (inc depth))
          sub-as-param (when (map? sub-form) (sub-form ::as))
          sub-param (when (qualified? sub-form) sub-form)]
      (assert (not (vector? sub-form))
              "Do not expect immediately nested lists in GraphQL queries")
      (doseq [[p1 p2] [[as-param sub-as-param]
                       [as-param sub-param]
                       [sub-param sub-as-param]]]
        (assert (or (nil? p1) (nil? p2))
              (str "Input format can't have "
                   (dequalify-kw p1)
                   " and " (dequalify-kw p2)
                   " both refer to the same value")))

      (assert (not= edge ::as)
              (str "Expect ?as to bind to a parameter, but "
                   form " found instead"))
      [[{:param param :symbol symbol :list true
         :path (list edge) :parent parent :depth depth}]
       other-dependents])

    ; only care about symbols that are used
    (contains? params-used form)
    [[{:param form
       :symbol (gensym (str (dequalify-kw form) \-))
       :path (if (= edge ::as) (list) (list edge))
       :parent parent
       :depth depth}]
     []]
    ; return nothing if form not interesting
    :else [[] []]))

(defn- extract-params
  "Generate dependency graph of how to access parameters within
   input format by combining output of extract-dependencies.
   Return a tuple of the result graph represented as a map from
   the id (i.e. :param) to the node and the root node."
  [form params-used]
  (let [[[root] params]
        (extract-dependencies form nil "input" params-used 0)
        result (reduce (fn [m p] (assoc m (:param p) p)) {}
                       (conj params root))]
    (doseq [param params-used]
      (assert (contains? result param)
              (str "Parameter " (dequalify-kw param)
                   " not found in input format")))
    [result root]))

(defn- traverse-dependencies
  "Generate the list of all nodes from the specified node up
   to the root."
  [graph param]
  (take-while (comp some? :parent graph)
              (iterate (comp :parent graph) param)))

(defn- create-get-in-binding [graph param]
  (let [{self :symbol path :path parent :parent} (graph param)
        source (-> parent graph :symbol)]
    (cond
      (empty? path) [self source]
      (= 1 (count path)) [self `(get ~source ~(first path))]
      :else [self `(get-in ~source [~@path])])))

; all create-* functions below return a tuple of the generated
; function body and a set of parameters that it use but has not
; bound

(declare create-inner-form)

(defn- create-let-expression
  [form param-paths dont-bind]
  (let [[inner-form unbound-params]
        (create-inner-form form param-paths dont-bind)
        bind-here
        (->> unbound-params
             (filter #(not (contains? dont-bind %)))
             (sort-by (comp :depth param-paths)))

        still-unbound (apply disj (set unbound-params) bind-here)
        new-params (mapcat #(create-get-in-binding param-paths %)
                           bind-here)
        result-form (if (empty? new-params) inner-form
                      `(let [~@new-params] ~inner-form))]
    [result-form still-unbound]))

(defn- create-for-expression
  [iter-params sub-form param-paths dont-bind]
  (assert (seq iter-params)
          "?for expressions can't have no iterative parameters")
  (let [dont-bind-below
        (->> iter-params
             (mapcat (partial traverse-dependencies param-paths))
             (filter (comp :list param-paths))
             (mapcat (partial traverse-dependencies param-paths))
             (into dont-bind))

        [let-exp unbound-params]
        (create-let-expression sub-form param-paths
                               dont-bind-below)
        bind-here (->> unbound-params
                       (filter (comp nil? dont-bind))
                       (sort-by (comp :depth param-paths)))

        still-unbound (apply disj unbound-params bind-here)
        make-binding (partial create-get-in-binding param-paths)
        make-bindings
        #(if ((param-paths (first %)) :list)
           (mapcat make-binding %)
           [:let (vec (mapcat make-binding %))])
        iter-list (mapcat make-bindings
                          (partition-by (comp :list param-paths)
                                        bind-here))
        result-form `(for [~@iter-list] ~let-exp)]
    (assert (seq bind-here)
            (str "Parameter" (when (> (count iter-list) 1) \s)
                 \space (str/join ", " iter-list)
                 " don't result in any iteration in ?for "
                 "expressions"))
    [result-form still-unbound]))

(defn- create-inner-form
  [form param-paths dont-bind]
  (assert (or (not (qualified? form))
              (contains? param-paths form))
          (str "Symbol " (dequalify-kw form)
               " not found in input format"))
  (cond
    (qualified? form)
    [((param-paths form) :symbol)
     (set (traverse-dependencies param-paths form))]

    ; [?for ?param] expressions
    (and (vector? form)
         (contains? #{::for ::for-lazy} (first form))
         (= (count form) 2))
    (let [list-params [(second form)]
          sub-form (second form)
          [for-exp params-used]
          (create-for-expression list-params sub-form
                                 param-paths dont-bind)]
      [(if (= ::for-lazy (first form)) for-exp `(vec ~for-exp))
       params-used])

    ; [?for [?params] sub-form] expressions
    (and (vector? form)
         (contains? #{::for ::for-lazy} (first form))
         (= (count form) 3))
    (let [list-params (second form)
          sub-form (last form)
          [for-exp params-used]
          (create-for-expression list-params sub-form
                                 param-paths dont-bind)]
      [(if (= ::for-lazy (first form)) for-exp `(vec ~for-exp))
       params-used])

    ; [?for [?params] sub-form1 sub-form2 ...] expressions
    (and (vector? form)
         (contains? #{::for ::for-lazy} (first form))
         (> (count form) 3))
    (let [list-params (second form)
          sub-form (vec (drop 2 form))
          [for-exp params-used]
          (create-for-expression list-params sub-form
                                 param-paths dont-bind)
          exp `(apply concat ~for-exp)]
      [(if (= ::for-lazy (first form)) exp `(vec ~exp))
       params-used])

    ; #{?for ?param} expressions
    (and (set? form)
         (contains? form (qualify ?for))
         (= (count form) 2))
    (let [list-params (disj form (qualify ?for))
          sub-form (first list-params)
          [for-exp params-used]
          (create-for-expression list-params sub-form
                                 param-paths dont-bind)]
      [`(set ~for-exp) params-used])

    ; #{?for ?param1 ?param2 ...} expressions
    (and (set? form)
         (contains? form (qualify ?for))
         (> (count form) 2))
    (let [list-params (disj form (qualify ?for))
          sub-form (vec list-params)
          [for-exp params-used]
          (create-for-expression list-params sub-form
                                 param-paths dont-bind)]
      [`(set (apply concat ~for-exp)) params-used])

    ; {?for [?params] ?key ?val} expressions
    (and (map? form)
         (contains? form (qualify ?for)))
    (let [list-params (form (qualify ?for))
          sub-form (vec (dissoc form (qualify ?for)))
          [for-exp params-used]
          (create-for-expression list-params sub-form
                                 param-paths dont-bind)]
      [`(into {} (apply concat ~for-exp)) params-used])

    ; recursively traverse ordinary data structures
    :else
    (let [recursive-result
          (walk #(create-inner-form % param-paths dont-bind)
                identity form)
          result (walk first identity recursive-result)
          ; if walk above did work, then there could be params
          ; to be extracted, else nothing
          params (if (not= result recursive-result)
                   (set (mapcat second recursive-result))
                   #{})]
      [result params])))

(defn check-path-reachable [path data]
  (cond
    (empty? path) true
    (= ::list (first path))
    (and (vector? data)
         (or (empty? data)
             (recur (rest path) (first data))))
    :else
    (and (map? data)
         (contains? data (first path))
         (recur (rest path) (data (first path))))))

(defn- generate-asserts [params-graph data-sym]
  (for [[param _]
        (sort-by (comp :depth second) params-graph)
        :when (-> param params-graph :parent some?)]
    (let [params (traverse-dependencies params-graph param)
          nodes (map params-graph (reverse params))
          path (mapcat (fn [{is-list :list path :path}]
                         (if is-list (concat path [::list]) path))
                       nodes)]
      `(assert (check-path-reachable [~@path] ~data-sym)
               ~(str "Parameter " (dequalify-kw param)
                     " not reachable")))))

(defn create-fn-expression [in-shape out-shape gen-asserts]
  (let [params-used (extract-bindings out-shape)
        [param-paths root] (extract-params in-shape params-used)
        [let-exp _]
        (create-let-expression out-shape param-paths
                               #{(root :param)})]
    `(fn [~(root :symbol)]
       ~@(if gen-asserts
           (generate-asserts param-paths (root :symbol))
           [])
       ~let-exp)))

(def gen-asserts (atom false))

(defmacro precompile [in-shape out-shape]
  (create-fn-expression (eval in-shape) (eval out-shape)
                        @gen-asserts))

(defn converter [in-shape out-shape]
  (eval (create-fn-expression in-shape out-shape @gen-asserts)))
