(defproject benrikuro "0.6.0"

  :url "https://github.com/ccfontes/benrikuro"

  :description "Clojure utility forms and aliases for some clojure.core forms."

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :scm {:name "git"
        :url "https://github.com/ccfontes/benrikuro"}

  :repositories [["clojars" {:creds :gpg}]]
  :deploy-repositories [["releases" :clojars] ["snapshots" :clojars]]

  :creds :gpg

  :dependencies [[org.clojure/clojure "1.5.1"] [prismatic/plumbing "0.4.1"]]

  :profiles {:dev {:plugins [[codox "0.8.10"]]}}

  :repl-options {:init-ns benri.kuro}

  :codox {:src-dir-uri "https://github.com/ccfontes/benrikuro/blob/master/"
          :src-linenum-anchor-prefix "L"})
