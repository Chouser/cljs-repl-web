(ns cljs-repl-web.config)

(def defaults
  (let [base-path ""]
    {:name "Clojurescript.io Website (DEV)"
     :production? false
     :base-path base-path
     :src-paths [(str base-path "/cljs-src")]
     :version-path (str base-path "/version.properties")
     :verbose-repl? true}))
