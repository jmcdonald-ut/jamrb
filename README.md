jamrb
=====

Jonathon Alexander McDonald's Ruby

Setup
-----

This project requires [ruby](https://www.ruby-lang.org) and
[Racket](https://racket-lang.org/) to run effectively.  I recommend following
their setup guides.
```sh
$ ruby -v # => ruby 2.3.0p...
$ racket -v # => Racket v6.1.1...
```

Next install bundler so you can install all ruby dependencies easily.
```sh
$ gem install bundler -N # -N flags for no documentation to be installed...
$ bundle -v # => Bundler version 1.11.2
```

Finally install the ruby dependencies for the project.
```sh
$ bundle install --binstubs
```

The sub-projects include a `Makefile` which will handle compiling the source
code into a binary.  Simply navigate to the desired sub-project and run:
```sh
$ make clean # => Remove any dirty files
$ make # => Install the binary from source
```

Testing
-------

Assuming you followed the setup instructions you can run the specs with:
```sh
$ ./bin/rspec
```

About
-----

This project a collection of smaller projects that together compile  
[ruby](https://www.ruby-lang.org).  Progress will be--is--slow as this is
simply a side project for learning and fun.

**NOTE: Currently this project does NOT compile ruby. It is in its infancy.**

### Structure

The outer directory of this project contains the smaller projects and a suite
of specs against them. The specs are located in `spec` as well as utility spec
files.

Sub-projects should include their own `README.md`, `Makefile`, and possibly
their own [git](https://git-scm.com/) repository. In general all source code for
the sub-project should be housed in `./source`. If the project generates an
executable it should output the binary to `./bin`.  

Below is a snapshot of the current project:
```
.
├── jamrb-lex
│   ├── bin
│   ├── source
│   └── test
│       └── rb-sx-tests
├── jamrb-sx
│   ├── bin
│   ├── source
│   └── test
└── spec
    └── jamrb-lex
```
