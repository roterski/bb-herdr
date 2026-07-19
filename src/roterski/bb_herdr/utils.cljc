(ns roterski.bb-herdr.utils
  (:require [clojure.string :as str]))

(defn join-lines
  [& lines]
  (->> lines
       (filter identity)
       (str/join "\n")))

^:rct/test
(comment
  (join-lines "hello"
              "world")
  ;;=> "hello\nworld"
  (join-lines "hello"
              nil
              "world")
  ;;=> "hello\nworld"
  (join-lines "hello"
              ""
              "world")
  ;;=> "hello\n\nworld"
  )
