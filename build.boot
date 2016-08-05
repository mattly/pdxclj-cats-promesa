(set-env!
 :source-paths #{"src"}
 :dependencies '[[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.namespace "0.3.0-alpha3"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [org.clojure/test.check "0.9.0"]
                 [funcool/cats "2.0.0"]
                 [funcool/promesa "1.4.0"]])

(deftask cider []
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[cider/cider-nrepl "0.12.0"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware])
  identity)

(require '[clojure.tools.namespace.repl :refer [refresh]])
(defn refresh-boot []
  (load-file "build.boot"))
