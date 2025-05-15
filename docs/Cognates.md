## Cognates

lparallel provides parallel versions of many Common Lisp functions and macros, each prefixed by ‘p’. They are the pmap family, preduce, and the following.

    pand
    pcount
    pcount-if
    pcount-if-not
    pdotimes
    pevery
    pfind
    pfind-if
    pfind-if-not
    pfuncall
    plet
    pnotany
    pnotevery
    por
    premove
    premove-if
    premove-if-not
    psome
    psort

They return the same results as their CL counterparts except in cases where parallelism must play a role. For instance premove behaves essentially like its CL version, but por is slightly different. or returns the result of the first form that evaluates to something non-nil, while por may return the result of any such non-nil-evaluating form.

plet is best explained in terms of its macroexpansion.

```lisp
(defpackage :example (:use :cl :lparallel))
(in-package :example)

(plet ((a (+ 3 4))
       (b (+ 5 6)))
  (list a b))

; => (7 11)
```

The plet form expands to

```lisp
(LET ((#:A725 (FUTURE (+ 3 4)))
      (#:B726 (FUTURE (+ 5 6))))
  (SYMBOL-MACROLET ((A (FORCE #:A725))
                    (B (FORCE #:B726)))
    (LIST A B)))
```

See Promises for an explanation of future and force. Since a future’s result is cached (a feature all promises share), references to a and b incur little overhead once their corresponding futures have finished computing.

There are four cognates which have no direct CL counterpart:

    plet-if
    pmaplist-into
    pmap-reduce
    preduce-partial
