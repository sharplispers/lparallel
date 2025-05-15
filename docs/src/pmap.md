## pmap

The pmap family consists of parallelized versions of the Common Lisp mapping functions, each denoted by a ‘p’ prefix. They are:

    pmap
    pmap-into
    pmapc
    pmapcan
    pmapcar
    pmapcon
    pmapl
    pmaplist
    pmaplist-into

All take the same arguments and produce the same results as their respective Common Lisp counterparts. pmaplist-into does not actually have a CL counterpart, but it does what you think it does.

By default pmaps operate on their input sequence(s) in chunks. That is, subsequences of the input sequence(s) are mapped in parallel rather than on a per-element basis. This strategy allows pmaps to be faster than their CL counterparts even in the realm of worst case scenarios (see benchmarks).

The default number of parallel-mapped parts is the number of worker threads (the number given to make-kernel). All pmaps accept a :parts keyword argument for specifying the number of parts explicitly.

```lisp
(defpackage :example (:use :cl :lparallel))
(in-package :example)

(pmap 'vector (lambda (x) (* x x)) :parts 2 '(3 4 5))
; => #(9 16 25)
```

(There is no ambiguity in the arguments because the :parts symbol is not a sequence.) When the number of parts is greater than or equal to the number of elements in the result sequence, the subdividing stage is elided and per-element parallelism is performed directly (an optimization).

In addition to :parts, all pmaps accept a :size option for specifying the number of elements to be mapped.

```lisp
(pmapcar 'identity :size 2 '(a b c d)) ; => (A B)

(map-into  (vector 1 2 3 4) 'identity '(a b))             ; => #(A B 3 4)
(pmap-into (vector 1 2 3 4) 'identity '(a b))             ; => #(A B 3 4)
(pmap-into (vector 1 2 3 4) 'identity :size 2 '(a b c d)) ; => #(A B 3 4)
```

As you probably know, map-into disregards the fill pointer (if one exists) of the result sequence when determining the result size. pmap-into behaves the same way, but also allows the result size to be determined by the :size argument. Like map-into, pmap-into will adjust the fill pointer of the result sequence after mapping is complete.

```lisp
(let ((vec (make-array 4 :fill-pointer 4 :initial-contents '(1 2 3 4))))
  ;; same as map-into
  (pmap-into vec 'identity '(a b))) ; => #(A B)

(let ((vec (make-array 4 :fill-pointer 4 :initial-contents '(1 2 3 4))))
  (pmap-into vec 'identity :size 2 '(a b c d))) ; => #(A B)
```

The :size argument also acts as an optimization for lists. Lists are not an ideal structure for parallel mapping because the subdivision process requires lengths to be known. When :size is given, all length calls are skipped.

Warning: the value of the :size option must be less than or equal to the length of the smallest sequence passed. It is unspecified what happens when that condition is not met.

As a rule of thumb, prefer arrays to lists where possible when using the pmap family of functions. In order to make array usage slightly more convenient, pmapcar accepts sequences. That is, (pmapcar ...) is an abbreviation for (pmap 'list ...).
