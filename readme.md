A toy Lisp loosely based on [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

TODO:

- [ ] error checking
- [x] repl
- [ ] builtins for IO
- [ ] builtins for list manipulation
- [ ] a basic standard library
- [x] map
- [x] reduce
- [x] apply builtin (?). like right now `(reduce + '(1 2 3))` doesn't work
- [ ] types: nil, vectors
- [ ] variadic functions
- [ ] pattern matching/destructuring
- [x] let
- [ ] basic shorthand/macros: , `(lambda (a1, …, aN))` -> `#( %1 … $N)`
- [x] `quote` -> `'`
- [x] recursion
- [ ] modules/namespaces
- [x] show AST
- [ ] maps

To hack:

    $ ./dev.sh

To run the tests:

    $ ./test.sh

To run the program:

    $ ./run.sh

