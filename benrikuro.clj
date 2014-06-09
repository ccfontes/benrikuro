(ns benrikuro
  (:require [clojure.reflect :refer [reflect]]
            [clojure.pprint :refer [print-table]]))

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

(defn ffilter
  "Returns the first item of coll for which (pred item) returns logical true.
   Consumes sequences up to the first match, will consume the entire sequence
   and return nil if no match is found."
  [pred coll] (一 (→?→ pred coll)))

; (map str "abc" (concat "de" (repeat "")))
(defn map-dregs [f & colls]
  ((fn map* [f colls]
     (lazy-seq
       (when (some seq colls)
         (cons (apply f (map first (filter seq colls)))
               (map* f (map rest colls))))))
   f colls))

;(defn map-dregs [f & colls]
;  (let [coll (partition-all (count ["a" "b"]) (concat ["a" "b"] ["c" "d" "e"]))]
;    (concat
;      (apply map str (butlast coll))
;      (last coll))))

(defn update-in*
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

(defn update-each
  "Update the values for each of the given keys in a map where f is a function that takes each
  previous value and the supplied args and returns a new value. Like update-in*, unchanged values
  are not re-assoc'd."
  [m keys f & args]
  (→v (fn [m key]
        (apply update-in* m [key] f args))
          m keys))

(ƒ get-members [some-type]
  (↠ (→ some-type reflect :members)
     (filter :exception-types)
     (sort-by :name)
     print-table))

(defn call-method
  "Calls a private or protected method.

   params is a vector of classes which correspond to the arguments to
   the method e

   obj is nil for static methods, the instance object otherwise.

   The method-name is given a symbol or a keyword (something Named)."
  [klass method-name params obj & args]
  (-> klass (.getDeclaredMethod (name method-name)
                                (into-array Class params))
      (doto (.setAccessible true))
      (.invoke obj (into-array Object args))))

(defn get-field
  "Access to private or protected field.  field-name is a symbol or
  keyword."
  [klass field-name obj]
  (-> klass (.getDeclaredField (name field-name))
      (doto (.setAccessible true))
      (.get obj)))
