benrikuro
-------
Clojure utility forms and aliases for some clojure.core forms.
<img height=320 src="http://upload.wikimedia.org/wikipedia/commons/8/82/Onigiri_at_a_convenience_store_by_typester_in_Kamakura.jpg"></img>

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
