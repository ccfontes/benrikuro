(ns benrikuro
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
(defcopy defnk plumbing/defnk)
(defcopy vmap plumbing/map-vals)
(defcopy <- plumbing/<-)
(defcopy fn-> plumbing/fn->)
(defcopy fn->> plumbing/fn->>)

(def #^{:macro true} ƒ #'defn)
(def #^{:macro true} λ #'fn)
(def #^{:macro true} → #'->)
(def #^{:macro true} ↠ #'->>)
(def #^{:macro true} ← #'<-)
(def #^{:macro true} ∨ #'or)
(def #^{:macro true} ∧ #'and)
(def #^{:macro true} ∃→ #'some->)
(def #^{:macro true} ∃↠ #'some->>)
(def #^{:macro true} λ→ #'fn->)
(def #^{:macro true} λ↠ #'fn->>)

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

(ƒ map-dregs [f & colls]
  ((fn map* [f colls]
     (lazy-seq
       (when (some seq colls)
         (cons (apply f (map first (filter seq colls)))
               (map* f (map rest colls))))))
   f colls))

(defn update
  "Updates the value in map m at k with the function f.

  Like update-in, but for updating a single top-level key.
  Any additional args will be passed to f after the value."
  ([m k f] (assoc m k (f (get m k))))
  ([m k f x1] (assoc m k (f (get m k) x1)))
  ([m k f x1 x2] (assoc m k (f (get m k) x1 x2)))
  ([m k f x1 x2 & xs] (assoc m k (apply f (get m k) x1 x2 xs))))

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

(ƒ str->stream [string] (→ string .getBytes clojure.java.io/input-stream))
(defcopy str→stream str->stream)

(def nilify (constantly nil))

(defn unlazy
  "Same as map/filter/reduce, but preserves the input data type."
  [core-f f coll]
  (into (empty coll) (core-f f coll)))

(defn kmap
  "same as map but applied to keys of hash maps only."
  [f m]
  (into {} (map (λ [[k v]] [(f k) v]) m)))

(ƒ get-members [some-type]
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

(ƒ get-field
  "Access to private or protected field.  field-name is a symbol or
  keyword."
  [klass field-name obj]
  (→ klass (.getDeclaredField (name field-name))
      (doto (.setAccessible true))
      (.get obj)))
