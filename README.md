# gql-format

A Clojure library for building GraphQL queries and transforming the output data in a declaritive format in the style of [Meander](https://github.com/noprompt/meander).

[Clojar](https://clojars.org/gql-format): `[gql-format "0.1.1"]`

## Introduction

Consider the GraphQL query below:
```clojure
(defn employee-info [state]
  (let [q "query($state: State){
             employees(state: $state){
               id
               name{
                 first
                 last
               }
               office{
                 location{
                   zip_code
                 }
               }
             }
           }"
       results (make-ajax q {"$state" state})]
     (->> (get-in results ["data" "employees"])
       (map (fn [em]
              {:id (em "id")
               :name (str (get-in em ["name" "first"]) \space (get-in em ["name" "last"]))
               :zip (get-in em ["office" "location" "zip_code"])}))
       (group-by :zip)))
```
Manually writing the code above has the following drawbacks:
* The query, being a raw string, has no syntax highlighting, no auto-indent, no warnings for mismatched braces, and is hard to manipulate.
* The syntax of GraphQL does not distinguish between a map and a list of maps, so one must frequently reference documentation when manipulating the results.
* APIs can be specified with very nested fields. When the query is long and indentation is not reliable, tracking the right path to the property you want through a `get-in` can be error-prone.
* Since the output of GraphQL is unlikely to be in the format that your internal functions want to consume the data, code needs to be introduced that massages the data into the desired format. 
* If you have a 5 levels deep query and you add a field 3 levels deep, it can be tricky to thread that new field through all your transformations to a spot you want in the final output.
* This code results in two separate calls to `(get em "name")`. Manually written code can have a trade-off between performance and conciseness.
* If you want to test that the data matches the shape you expect (say for the list vs map problem above), that needs to be written separately despite the structure of the data already being present in the query.

The equivalent code using this library for this example is:
```clojure
(require '[gql-format.core :as gf])
(def query-format
  (gf/qualify
    {"employees" [:state "$state"
                  {?as ?employee
                   "id" ?id
                   "name" {"first" ?first-name
                           "last" ?last-name}
                   "office" {"location" {"zip_code" ?zip}}}]}))
(def query-string (gf/query {:$state "State"} query-format))
(def desired-format
  (gf/qualify
    `(group-by :zip
       [?for [?employee]
        {:id ?id
         :name (str ?first-name ?last-name)
         :zip ?zip}])))
(def formatter (gf/converter query-format desired-format))
(defn employee-info [state]
  (-> (make-ajax query-string {"$state" state})
    (get "data")
    formatter)
```
You just need to specify how you want output to look, and this library generates a function that converts the query results into that desired format. As a side-effect of telling the library how to extract fields from a query, you also get a query builder for free.

## Usage
See `test/gql_format/core_test.clj` for examples of usage.

### Building a query
The function `gf/query` takes in two parameters. The first is a list of parameters for the query in the form `{:$param "Type"}`.
The second is a map that represents the query body. Consider the example below:
```clojure
{"field" {:field-param 1
          "value" ?value
          "subfield" [{?as ?sub-field
                       "subval1" ?sub-val
                       "subval2" ?_}]}}
```
That would produce a GraphQL query
```
field(field-param: 1){
  value
  subfield{
    subval1
    subval2
  }
}
```
Keywords keys are interpreted as parameters, and string keys are interpreded as fields. The map value for a field can have three forms. One is the parameter name this is bound to for the library to generate the conversion function. Or it can be a map that is treated gets recursively parsed as a query. It can also be a vector containing one map that gets treated as a map. This is necessary for the conversion to distinguish between elements and lists of elements in a GraphQL query. An `?as` parameter is bounded to the map or vector as a whole, and is also ignored during query generation.

There are two ways to specify a binding parameter for `gf/converter`. One is to use a namespaced keyword `::gf/param`. Alternatively, the macro `gf/qualify` recursively searches for all occurances of `?param` and replaces it with `::gf/param`. This is meant to align with the syntax of Meander.

Obviously, `?as` is a reserved keyword and can't be used as a parameter name. Other reserved keywords are `?for` and `?for-lazy`.

### Specifying the desired output format

The function `gf/converter` takes in the query format used to generated the query string and an output format form, and return a function.
That function would then take an input in the form of the query format and transform it into the output format. The output format can be anything that would fit within a function body, with the following modifications:

* Any instances of parameter bindings in the from `?param` would be replaced by the corresponding value extraction from the function's input.
* A vector literal in the form `[?for [?a ?b] ?c ?d]` would be replaced by a vector containing instances of `?c` and `?d` for each valid bindings of `?a` and `?b`.
* `[?for ?c]` where `?c` is the only parameter in the vector is shorthand for `[?for [?c] ?c]`.
* Use `?for-lazy` instead of `?for` would replace the vector with a lazy sequence.
* `{?for [?a ?b] ?c ?d}` would be replaced by a may where the keys are instances of `?c` and their values corresponding instances of `?d` for each valid binding of `?a` and `?b` from input data.
* `#{?for ?c}` would be replaced by a set containing all instances of `?c`. Since there's no way to specify the iteration binding list (the `[?a ?b]` above), only the shorthand syntax is provided. If a more complex data structure is desired, you can do `` `(set [?for [?a ?b] (+ ?c ?d)])``. Observe the back-tick is needed to prevent the form being immediately evaluated.

## Implementation and performance

`gf/converter` creates a function based on the forms in its perameter and passes it to `eval`. Using the example from introduction, `(pprint (gf/create-fn-expression query-format desired-format))` outputs the following:
```clojure
(clojure.core/fn
 [??input-9778]
 (clojure.core/group-by
  :zip
  (clojure.core/vec
   (clojure.core/for
    [?employee-9779 (clojure.core/get ??input-9778 "employees")]
    (clojure.core/let
     [??name-9781
      (clojure.core/get ?employee-9779 "name")
      ?id-9780
      (clojure.core/get ?employee-9779 "id")
      ?first-name-9782
      (clojure.core/get ??name-9781 "first")
      ?last-name-9783
      (clojure.core/get ??name-9781 "last")
      ?zip-9786
      (clojure.core/get-in
       ?employee-9779
       ["office" "location" "zip_code"])]
     {:name (clojure.core/str ?first-name-9782 ?last-name-9783),
      :zip ?zip-9786,
      :id ?id-9780})))))
```
This allows anyone to stop using this library whenever they want by replacing `gf/converter` with its output.

Assuming that `gf/converter` is only called once at start-up, and the resulting function is reused, the generated function above has roughly equivalent performance to a hand-written somewhat optimized function of the same functionality. See `test/gql_format/performance_test.clj` for benchmarking.

There is also another macro `gf/percompile` that does the same thing as `gf/converter` at compile-time to enable a compiler to potentially further optimize the generated code. With the JVM benchmark, there is no observeable performance difference, and other platforms like ClojureScript is untested.

## Data validation
A sequence of asserts can be generated as a part of the `gf/converter` output that validates the input of the function conforms to the the format it's expected to see based on `query-format` above. To enable it, do `(reset! gf/gen-asserts true)`. To see what code would be generated, use the `gf/create-fn-expression` above with the third parameter being `true`. The output for the example would be:
```clojure
(clojure.core/fn
 [??input-9778]
 (clojure.core/assert
  (gql-format.core/check-path-reachable
   ["employees" :gql-format.core/list]
   ??input-9778)
  "Parameter ?employee not reachable")
 (clojure.core/assert
  (gql-format.core/check-path-reachable
   ["employees" :gql-format.core/list "id"]
   ??input-9778)
  "Parameter ?id not reachable")
 (clojure.core/assert
  (gql-format.core/check-path-reachable
   ["employees" :gql-format.core/list "name"]
   ??input-9778)
  "Parameter ??name-9781 not reachable")
 (clojure.core/assert
  (gql-format.core/check-path-reachable
   ["employees" :gql-format.core/list "name" "first"]
   ??input-9778)
  "Parameter ?first-name not reachable")
 (clojure.core/assert
  (gql-format.core/check-path-reachable
   ["employees" :gql-format.core/list "name" "last"]
   ??input-9778)
  "Parameter ?last-name not reachable")
 (clojure.core/assert
  (gql-format.core/check-path-reachable
   ["employees" :gql-format.core/list "office" "location" "zip_cod
e"]
   ??input-9778)
  "Parameter ?zip not reachable")
 (clojure.core/group-by ...
```
where `gf/check-path-reachable` is implemented as
```clojure
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
```

## License

Copyright © 2020 

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
