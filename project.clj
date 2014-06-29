(defproject benrikuro "0.4.0"

  :url "https://github.com/ccfontes/benrikuro"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [prismatic/plumbing "0.3.0"]]

  :profiles {:dev {:plugins [[codox "0.8.9"]]}}

  :repl-options {:init-ns benri.kuro}

  :codox {:src-dir-uri "https://github.com/ccfontes/benrikuro/blob/master/"
          :src-linenum-anchor-prefix "L"})