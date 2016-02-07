`jamrb-lex`
==========

Library for tokenizing ruby code into Racket style S-Expressions.

Installation
------------

The only dependency for `jamrb_lex` is [Racket v6.3](https://racket-lang.org/).

```sh
$ racket -v # => Racket v6.3?  Great!
$ make # => Outputs to ./bin/jamrb_lex
```

Testing
-------

The root project, [`jamrb`](https://github.com/jmcdonald-ut/jamrb), includes a
utility for all of the integration testing. For each ruby file in
`test/rb-sx-tests` with a corresponding `.rb.out` file rspec will run
`jamrb_lex < #{FILE}` and compare the output against the corresponding out file.

For unit testing proper you can simply navigate to `test` and run the tests
using racket. For example:

```sh
$ cd ./test
$ racket utility.test.rkt
13 success(es) 0 failure(s) 0 error(s) 13 test(s) run
0
```

Structure
---------

* `source/abbrevs.rkt`
  * Contains all regex pattern abbreviations used to match tokens in plain text.
* `source/jamrb.rkt`
  * Primary application file.
  * Parses command line inputs.
  * Writes out the results of tokenizing some input to standard out.
* `source/lexers.rkt`
  * Contains simpler lexers that help to tokenize ruby.
  * For example it contains a lexer for strings, a lexer for ids, and so on.
* `source/state.rkt`
  * Holds data structures that track information while lexing.
  * For example this holds a stack of opening parens.
  * When a closing paren is lexed, the stack is popped.
  * If a closing value doesn't pair with the value popped, an error is raised.
* `source/utility.rkt`
  * Provides useful functions for working with IO and tokenizing data.
