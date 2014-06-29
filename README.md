benrikuro
-------
Clojure utility forms and aliases for some clojure.core forms.

### Installing
-------
Add the following entry to the `:dependencies` vector of your `project.clj` file:
[![clojars version](https://clojars.org/benrikuro/latest-version.svg?raw=true)]
(https://clojars.org/benrikuro)

### Crafting
-------
```clj
user=> (use 'benri.kuro)
nil
user=> (update-each {:a 1 :b 2 :c 3} [:a :b] inc)
{:a 2, :c 3, :b 3}
user=> (update-multi {:a 1 :b 2 :c 3} {:a inc :b dec})
{:a 2, :c 3, :b 1}
```
Also read the [API](http://ccfontes.github.io/benrikuro/benri.kuro.html).

### Gentle contributions
-------
Were made by many many people.
