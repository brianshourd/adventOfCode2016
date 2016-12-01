# Advent of Code 2016

My solutions to the [http://adventofcode.com](http://adventofcode.com) 2016
problems, in Haskell. Why? To keep my Haskell skills sharp, to learn some new
libraries, and for fun!

To run these, install [stack](http://haskellstack.org), then run

```
stack setup
stack test        # To run tests
stack build       # To build
stack exec main   # To see all answers
```

It is possible to do some highly effective TDD with the following command, which
continuosly watches the file system and rebuilds/reruns tests whenever a file
changes.

```
stack test --fast --file-watch
```
