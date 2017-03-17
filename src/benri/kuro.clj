(ns benri.kuro
  (:require [clojure.reflect :refer [reflect]]
            [clojure.pprint :refer [print-table]]
            [plumbing.core :as plumbing]))

(defmacro defcopy
  "Defines a copy of a var: a new var with the same root binding (if
   any) and similar metadata. The metadata of the copy is its initial
   metadata (as provided by def) merged into the metadata of the original.
   source: same as defalias from clojure 1.2 and downwards."
  ([name orig]
  `(do
     (alter-meta!
      (if (.hasRoot (var ~orig))
        (def ~name (.getRawRoot (var ~orig)))
        (def ~name))
      ;; When copying metadata, disregard {:macro false}.
      ;; Workaround for http://www.assembla.com/spaces/clojure/tickets/273
      #(conj (dissoc % :macro)
             (apply dissoc (meta (var ~orig)) (remove #{:macro} (keys %)))))
     (var ~name)))
  ([name orig doc]
   (list `defcopy (with-meta name (assoc (meta name) :doc doc)) orig)))

(defcopy fnk plumbing/fnk)
(defcopy λk fnk)

(defcopy defnk plumbing/defnk)
(defcopy ƒk defnk)

(defcopy map-vals plumbing/map-vals)

(defcopy <- plumbing/<-)
(def #^{:macro true} ← #'<-)

(defcopy fn-> plumbing/fn->)
(def #^{:macro true} λ→ #'fn->)

(defcopy fn->> plumbing/fn->>)
(def #^{:macro true} λ↠ #'fn->>)

(defcopy ?> plumbing/?>)
(defcopy ?>> plumbing/?>>)

(def #^{:macro true} ƒ #'defn)
(def #^{:macro true} λ #'fn)
(def #^{:macro true} → #'->)
(def #^{:macro true} ↠ #'->>)

(def #^{:macro true} ∨ #'or)
(def #^{:macro true} ∧ #'and)
(def #^{:macro true} ∃→ #'some->)
(def #^{:macro true} ∃↠ #'some->>)


;(def χ def)
(def →λ partial)
(def ∘ comp)
(def →v reduce)
(def →?→ filter)
(def 一 first)
(def 二 second)
(def ∁ complement)
(def ∃ some)
(def ∃λ some-fn)
(def ∄ (∁ ∃))
(def ¬ not)
(def ≠ not=)
(def ⚛ atom)

(ƒ ffilter
  "Returns the first item of coll for which (pred item) returns logical true.
   Consumes sequences up to the first match, will consume the entire sequence
   and return nil if no match is found."
  [pred coll] (一 (→?→ pred coll)))

(ƒ first-truthy
  "Returns the first item in coll for which (pred item) is not nil.
   Consumes sequences up to the first match, will consume the entire sequence
   and return nil if no match is found."
  [coll] (ffilter identity coll))

(ƒ keep-first
  "Returns the first (pred item) for which value is not nil.
   Consumes sequences up to the first match, will consume the entire sequence
   and return nil if no match is found."
  [pred coll] (一 (keep pred coll)))

(ƒ mapm
  "Returns a map consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments."
  ([f coll]
     (-> (reduce (fn [v o] (conj! v (f o))) (transient {}) coll)
         persistent!))
  ([f c1 c2] (into {} (map f c1 c2)))
  ([f c1 c2 c3] (into {} (map f c1 c2 c3)))
  ([f c1 c2 c3 & colls] (into {} (apply map f c1 c2 c3 colls))))

(ƒ map-dregs [f & colls]
  "Like map but when there is a different count between colls, applies input fn
   to the coll values until the biggest coll is empty."
  ((fn map* [f colls]
     (lazy-seq
       (if-let [non-empty-colls (seq (filter seq colls))]
         (let [first-items (map first non-empty-colls)
               rest-colls (map rest non-empty-colls)]
           (cons (apply f first-items)
                 (map* f rest-colls))))))
  f colls))

(ƒ map-keys
  "Applies f to all the keys in the map."
  [f m]
  (reduce-kv (fn [m k v] (assoc m (f k) v))
             {}
             (or m {})))

(ƒ update-in*
  "Updates a value in a nested associative structure, where ks is a sequence of keys and f is a
  function that will take the old value and any supplied args and return the new value, and returns
  a new nested structure. If any levels do not exist, hash-maps will be created. This implementation
  was adapted from clojure.core, but the behavior is more correct if keys is empty and unchanged
  values are not re-assoc'd."
  [m keys f & args]
  (if-let [[k & ks] (seq keys)]
    (let [old (get m k)
          new (apply update-in* old ks f args)]
      (if (identical? old new)
        m
        (assoc m k new)))
     (apply f m args)))

(ƒ update-each
  "Update the values for each of the given keys in a map where f is a function that takes each
  previous value and the supplied args and returns a new value. Like update-in*, unchanged values
  are not re-assoc'd."
  [m keys f & args]
  (→v (fn [m key]
        (apply update-in* m [key] f args))
          m keys))

(ƒ update-multi
  "Updates multiple keys of a map with multiple fns using a map of key/fn pairs."
  [m fn-m]
  (merge
    m
    (∃↠ m (#(map (fn [[k f]] [k (f (k %))]) fn-m)) (into {}))))

(ƒ str->stream [string] (→ string .getBytes clojure.java.io/input-stream))
(defcopy str→stream str->stream)

(def nilify (constantly nil))

(ƒ or-> [x & args]
  "Same as -> but defaults to the initial value if the result is falsey."
  (or (eval `(-> ~x ~@args)) x))

(ƒ ->members [some-type]
  (↠ (→ some-type reflect :members)
     (filter :exception-types)
     (sort-by :name)
     print-table))

(ƒ call-method
  "Calls a private or protected method.

   params is a vector of classes which correspond to the arguments to
   the method e

   obj is nil for static methods, the instance object otherwise.

   The method-name is given a symbol or a keyword (something Named)."
  [klass method-name params obj & args]
  (→ klass (.getDeclaredMethod (name method-name)
                                (into-array Class params))
      (doto (.setAccessible true))
      (.invoke obj (into-array Object args))))

(ƒ ->field
  "Access to private or protected field. field-name is a symbol or
  keyword."
  [klass field-name obj]
  (→ klass (.getDeclaredField (name field-name))
      (doto (.setAccessible true))
      (.get obj)))

(ƒ debug
  "Print class and value information for input Var."
  [arg]
  (println "debug")
  (println "class: " (class arg))
  (println "value: " arg))

(ƒ thrush-debug
  "Print class and value information for input Var inside '->' or '->>'."
  [arg]
  (println "trush debug")
  (println "class: " (class arg))
  (println "value: " arg)
  arg)

(ƒ fskip [f in ret]
  "Wraps f so that it skips computation when
   first input is equal to in, returning ret.
   Useful e.g., for input empty collections."
  (fn [& args]
    (if (= (first args) in)
      ret
      (apply f args))))

(ƒ butlast-last [& args]
  [(butlast args) (last args)])

(ƒ map-first-nth
  "Map only first nth elements of coll."
  [n f coll]
  (concat (map f (take n coll))
          (drop n coll)))

(ƒ interpose-by
  "Same as interpose, but sep string is the
   return of f with adjacent items as inputs."
  [f coll]
  (→ #(identity [%1 (f %1 %2)])
     (mapcat coll (rest coll))
     (concat [(last coll)])))

(ƒ slice
  "Like subvec, but for any collection."
  ([coll start end] (↠ coll (take end) (drop start)))
  ([coll start] (drop start coll)))

(ƒ ^String trim-unicode
  "Removes whitespace from both ends of string."
  [^CharSequence s]
  (let [len (.length s)]
    (loop [rindex len]
      (if (zero? rindex)
        ""
        (if (or (Character/isWhitespace (.charAt s (dec rindex)))
                (= "　" (.charAt s (dec rindex))))
          (recur (dec rindex))
          ;; there is at least one non-whitespace char in the string,
          ;; so no need to check for lindex reaching len.
          (loop [lindex 0]
            (if (Character/isWhitespace (.charAt s lindex))
              (recur (inc lindex))
              (.. s (subSequence lindex rindex) toString))))))))

(ƒ last-subs
  "Like take-last, but for strings."
  [n s]
  (->> s (take-last n) (apply str)))

(ƒ drop-last-subs
  "Like drop-last, but for strings."
  [n s]
  (->> s (drop-last n) (apply str)))

(defn find-pos
  "Find needle positions in a haystack."
  [needle haystack]
  (keep-indexed
    #(if (= %2 needle) %1)
    haystack))
