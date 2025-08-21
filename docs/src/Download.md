## Download

The easiest way to obtain lparallel is with [Quicklisp](http://www.quicklisp.org/beta/).

```lisp
(ql:quickload :lparallel)
```

This will download, compile, and load lparallel along with its dependency bordeaux-threads. Alternatively, it may be downloaded manually from the following links:

-   lparallel: [release](https://github.com/sharplispers/lparallel/tags), [repository](https://github.com/sharplispers/lparallel)
-   bordeaux-threads: [release](https://common-lisp.net/project/bordeaux-threads/releases/), [repository](https://github.com/sionescu/bordeaux-threads)

lparallel should run on any Common Lisp implementation supported by bordeaux-threads. The following implementations successfully pass the lparallel test suite:

-   ABCL
-   Allegro
-   Clozure
-   ECL
-   LispWorks
-   SBCL

To run the test suite,

```lisp
(ql:quickload :lparallel-test)
(lparallel-test:execute)
```

To run benchmarks,

```lisp
(ql:quickload :lparallel-bench)
(lparallel-bench:execute N)
```

where *N*  is the number of processors/cores.

