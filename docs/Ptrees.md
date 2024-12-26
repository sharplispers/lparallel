Ptrees

In cases where futures must wait upon the results of other futures, it may be more suitable to use a ptree instead. A ptree also has built-in support for retrying failed computations.

A ptree is a computation represented by a tree together with functionality to execute the tree in parallel. The simplest way to build and execute a ptree is with the ptree macro. Its syntax matches that of flet.

(defpackage :example (:use :cl :lparallel))
(in-package :example)

(ptree ((area   (width height) (* width height))
        (width  (border)       (+ 7 (* 2 border)))
        (height (border)       (+ 5 (* 2 border)))
        (border ()             1))
  area)

; => 63

This performs a parallelized version of the computation

(* (+ 7 (* 2 1))
   (+ 5 (* 2 1)))

; => 63

Each function in the ptree macro corresponds to a node in the generated tree. The relationships between the nodes are determined by the parameter names. In this example the area node has two child nodes labeled width and height; the width and height nodes share the same child node named border; and the border node has no children.

Each node contains a function and a result. The arguments passed to a node’s function are the respective results of its child nodes. The function result is stored in the node.

The function associated with a ptree node should be a pure function with regard to that ptree. It should depend only on its parameters and should not produce side-effects that impact other functions in the ptree. Otherwise, the result of the ptree computation is not well-defined.

Futures could also be used parallelize our example computation.

(let* ((border (future 1))
       (width  (future (+ 7 (* 2 (force border)))))
       (height (future (+ 5 (* 2 (force border)))))
       (area   (future (* (force width) (force height)))))
  (force area))

; => 63

What is the purpose of ptrees if futures can do the same thing? Futures are inadequate for large trees with interconnected relationships. A worker thread is effectively hijacked whenever a future waits on another future. A new temporary worker could be spawned to compensate for each worker that enters a waiting state, however in general that is an expensive solution which does not scale well.

The underlying issue is that futures have no knowledge of the computation tree in which they participate. Futures are simple and stupid; they work fine on their own but not in the context of interconnected futures. The solution is to describe the computation explicitly with a ptree. By examining the node relationships, a ptree is able to avoid the problem of blocked workers caused by futures.

Ptrees may be built dynamically as follows.

(let ((tree (make-ptree)))
  (ptree-fn 'area   '(width height) (lambda (w h) (* w h))       tree)
  (ptree-fn 'width  '(border)       (lambda (b)   (+ 7 (* 2 b))) tree)
  (ptree-fn 'height '(border)       (lambda (b)   (+ 5 (* 2 b))) tree)
  (ptree-fn 'border '()             (lambda ()    1)             tree)
  (call-ptree 'area tree))

; => 63

This code resembles the expansion of the ptree macro example above. Note that a node identifier need not be a symbol; any object suitable for eql comparison will do.

clear-ptree restores the tree to its original uncomputed state. clear-ptree-errors restores to the last pre-error state.

If the functions in a ptree themselves make use of parallelism—for instance if a node function calls pmap—then consider using a separate kernel for node computations by binding *ptree-node-kernel* to a kernel instance.
