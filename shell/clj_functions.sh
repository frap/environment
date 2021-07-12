# Starts a Clojure repl
function rebel-clj() {
  clojure -Sdeps "{:deps {com.bhauman/rebel-readline {:mvn/version \"0.1.4\"} $@}}" -m rebel-readline.main
}

# Starts a browser REPL
function rebel-cljs() {
  clojure -Sdeps "{:deps {com.bhauman/figwheel-main {:mvn/version \"0.1.9\"} com.bhauman/rebel-readline-cljs {:mvn/version \"0.1.4\"} $@}}" -m figwheel.main
}
