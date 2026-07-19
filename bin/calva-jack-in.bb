#!/usr/bin/env bb

(require '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[babashka.process :as p])

(let [[aliases-str nrepl-version cider-version] (if (= (count *command-line-args*) 3)
                                                  *command-line-args*
                                                  [nil (first *command-line-args*) (second *command-line-args*)])
      nrepl-version (or nrepl-version "1.5.1")
      cider-version (or cider-version "0.58.0")
      alias-keys   (when aliases-str
                     (->> (str/split (str/replace aliases-str "," "") #":")
                          (remove str/blank?)
                          (map keyword)))
      deps-edn     (edn/read-string (slurp "deps.edn"))
      has-main?    (some #(get-in deps-edn [:aliases % :main-opts]) alias-keys)
      aliases-flag (str "-M" (str/replace (or aliases-str "") "," ""))
      sdeps        (str "{:deps {nrepl/nrepl {:mvn/version \"" nrepl-version "\"}"
                        " cider/cider-nrepl {:mvn/version \"" cider-version "\"}}}")
      load-env-cmd "bin/load-env-exec"
      cmd          (cond-> [load-env-cmd "clojure" "-Sdeps" sdeps aliases-flag]
                     (not has-main?)
                     (into ["-m" "nrepl.cmdline" "--middleware" "[cider.nrepl/cider-middleware]"]))]
  (println (str/join " " cmd))
  (p/exec cmd))
