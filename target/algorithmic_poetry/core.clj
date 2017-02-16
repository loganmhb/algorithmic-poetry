(ns algorithmic-poetry.core
  (:require [clojure.string :as str]))

(def mb-txt (slurp "resources/moby_dick.txt"))

(defn text->words [text]
  (clojure.string/split text #" |\n|:|\.|;|\-|,|\"|\!|'|\?"))

(def mb-words
  (filter (partial re-find #"[a-zA-Z]") ; remove numbers, punctuation, spaces
          (text->words mb-txt))  )


(defn words [text]
  (filter (partial re-find #"[a-zA-Z]") ; remove numbers, punctuation, spaces
          (text->words text)))

(defn transitions [words]
  (partition-all 3 1 (map clojure.string/lower-case words)))

(defn transition-map [transitions]
  (reduce (fn [lookup-map transition]
            (let [[a b c] transition]
              (update lookup-map [a b] conj c)))
          {}
          transitions))

(def mb-transitions (partition-all 3 1 (map clojure.string/lower-case mb-words)))

(def mb-transition-map (reduce (fn [lookup-map transition]
                                 (let [[a b c] transition]
                                   (update lookup-map [a b] conj c)))
                               {}
                               mb-transitions))


(defn generate-text
  ([lookup-map]
   (generate-text lookup-map (rand-nth (keys lookup-map))))
  ([lookup-map word-pair]
   (let [next-pair [(last word-pair)
                    (rand-nth (get lookup-map word-pair))]]
     (cons (first word-pair)
           (lazy-seq (generate-text lookup-map next-pair))))))


(defn generate-excerpt-for-source-string [s]
  (let [lookup (-> s
                   words
                   transitions
                   transition-map)]
    (str/join " " (take 5000 (generate-text lookup)))))


(generate-excerpt-for-source-string mb-txt)
