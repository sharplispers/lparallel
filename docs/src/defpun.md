## defpun

Using plet is often a natural way to add parallelism to an algorithm. The result of doing so may be disappointing, however. Consider the classic Fibonacci example:

```lisp
(defpackage :example (:use :cl :lparallel))
(in-package :example)

(defun fib (n)
  (if (< n 2)
      n
      (let ((a (fib (- n 1)))
            (b (fib (- n 2))))
        (+ a b))))

(defun pfib-slow (n)
  (if (< n 2)
      n
      (plet ((a (pfib-slow (- n 1)))
             (b (pfib-slow (- n 2))))
        (+ a b))))
```

Living up to its name, pfib-slow is slow. Since plet spawns parallel tasks each time, and since addition is cheap, the overhead of task creation, scheduling, and execution will dominate.

```lisp
(time (fib 25))
=> 75025
Evaluation took:
  0.002 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  0.00% CPU
  6,912,680 processor cycles
  0 bytes consed

(time (pfib-slow 25))
=> 75025
Evaluation took:
  0.028 seconds of real time
  0.096006 seconds of total run time (0.096006 user, 0.000000 system)
  342.86% CPU
  93,778,257 processor cycles
  29,136,576 bytes consed
```

How do we fix this? One way is to create fewer tasks by partitioning the computation into larger chunks.

```lisp
(time (pmap-reduce 'fib '+ #(21 22 22 23)))
=> 75025
Evaluation took:
  0.001 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  0.00% CPU
  2,771,120 processor cycles
  96 bytes consed
```

In general it may not be easy to subdivide a computation and then glue the results together, as we have done here. The purpose of defpun is to handle this for us. defpun has the syntax and semantics of defun.

```lisp
(defpun pfib (n)
  (if (< n 2)
      n
      (plet ((a (pfib (- n 1)))
             (b (pfib (- n 2))))
        (+ a b))))
```

The above code defines the pfib function.

```lisp
(time (pfib 25))
=> 75025
Evaluation took:
  0.001 seconds of real time
  0.004000 seconds of total run time (0.004000 user, 0.000000 system)
  400.00% CPU
  2,601,638 processor cycles
  16,560 bytes consed
```

See benchmarks for more accurate measurements. Note that a high optimization level inside the defpun form may be required in order to obtain significant speedup.

How does defpun work? The plet macro has a local definition inside defpun. It expands into two distinct versions of its input: one version is a regular let form and the other is similar to the global plet but with logic added. The strategy resembles Cilk, where a “fast clone” and a “slow clone” are created from the input code. If we imagine a computation as one large tree, the fast clone is responsible for efficiently computing a given subtree while the slow clone is responsible for passing subtrees to the fast clone as parallel tasks and combining the results.
