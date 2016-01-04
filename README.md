# repl

A Clojure library to help explore potentially large data structures in the repl.

## Installation

Clone the repo and run

```
$ lein install
```

to put it in your local maven repo.

Then add the dependency to your `project.clj` file. I usually put it
in the `:dev` profile.

```
:dev  {:dependencies [[com.inferstructure/repl "0.1.0-SNAPSHOT"]]}
```

## Usage

`require` it in a repl:
```clojure
(require '[com.inferstructure.repl :as r])
```

and use it

```clojure
(r/explore 10 2 (range))
;=> (0 1 "more ...")
```

## License

Copyright © 2015-2016 Bret Young

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
