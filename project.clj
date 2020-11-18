(defproject gql-format "0.1.0"
  :description "A declaritive data formatting library for constructing GraphQL queries and transforming its results."
  :url "https://github.com/QikLiang/GraphQL-format.clj"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  ;:global-vars {*assert* false}
  :profiles {:dev {:dependencies [[criterium "0.4.5"]]}}
  :repl-options {:init-ns gql-format.core})
