(defproject om-tops "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [ring/ring "1.3.1"]
                 [compojure "1.1.8"]
                 [fogus/ring-edn "0.2.0"]]

  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]
            [com.cemerick/clojurescript.test "0.3.1"]]

  :source-paths ["src/clj"]

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "0.0-2311"]
                                  [com.cemerick/clojurescript.test "0.3.1"]
                                  [com.cemerick/double-check "0.5.7"]
                                  [om "0.7.1"]
                                  [com.facebook/react "0.11.1"]
                                  [ankha "0.1.3"]
                                  [prismatic/dommy "0.1.3"]]}}
  :cljsbuild
  {:builds
   [{:id "dev"
     :source-paths ["src/cljs"]
     :compiler {:output-to "resources/public/js/main.js"
                :output-dir "resources/public/js/out"
                :optimizations :none
                :source-map true}}
    {:id "prod"
     :source-paths ["src/cljs"]
     :compiler {:output-to "resources/public/js/main-prod.js"
                :optimizations :advanced
                :pretty-print false
                :preamble ["react/react.min.js"]
                :externs ["react/externs/react.js"]}}
    {:id "test"
     :source-paths ["src/cljs" "test/cljs"]
     :notify-command ["phantomjs" :cljs.test/runner
                      "test/vendor/es5-shim.js"
                      "test/vendor/es5-sham.js"
                      "test/vendor/console-polyfill.js"
                      "window.literal_js_was_evaluated=true"
                      "target/cljs/testable.js"]
     :compiler {:output-to "target/cljs/testable.js"
                :preamble ["public/js/react-0.11.1.js"]
                :optimizations :whitespace
                :pretty-print true}}]

   :test-commands {"unit" ["phantomjs" :runner
                           "test/vendor/es5-shim.js"
                           "test/vendor/es5-sham.js"
                           "test/vendor/console-polyfill.js"
                           "window.literal_js_was_evaluated=true"
                           "target/cljs/testable.js"]}})
