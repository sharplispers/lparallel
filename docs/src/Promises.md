## Promises

A promise is a receptacle for a result which is unknown at the time it is created. To fulfill a promise is to give it a result. The value of a promise is obtained by forcing it.

```lisp
(defpackage :example (:use :cl :lparallel))
(in-package :example)

(let ((p (promise)))
  (fulfilledp p) ; => nil
  (fulfill p 3)
  (fulfilledp p) ; => t
  (force p))

; => 3
```

A promise may be successfully fulfilled only once, after which force will forever return the same result. If fulfill is successful it returns true, otherwise it returns false indicating the promise is already fulfilled (or in the process of being fulfilled). When force is called on an unfulfilled promise, the call will block until the promise is fulfilled.

A future is a promise which is fulfilled in parallel. When a future is created, a parallel task is made from the code passed.

```lisp
(let ((f (future
           (sleep 0.2)
           (+ 3 4))))
  (fulfilledp f) ; => nil
  (sleep 0.4)
  (fulfilledp f) ; => t
  (force f))

; => 7
```

Here are two futures competing to fulfill a promise:

```lisp
(let* ((p (promise))
       (f (future
            (sleep 0.05)
            (fulfill p 'f-was-here)))
       (g (future
            (sleep 0.049999)
            (fulfill p 'g-was-here))))
  (list (force p) (force f) (force g)))

; => (F-WAS-HERE T NIL) or (G-WAS-HERE NIL T)
```

Whichever result appears is dependent upon your system. Note the return value of fulfill indicating success or failure.

Importantly, fulfill is a macro. When we consider giving fulfill an actual calculation to perform instead of an immediate value like 3, the reason for fulfill being a macro should be clear. If a promise is already fulfilled then we do not want the code passed to fulfill to be needlessly executed.

A speculation—created by speculate—is a low-priority future whose associated task is executed only when those of regular futures are not pending.

Like futures and speculations, a delay is also a promise associated with some code. However instead of being fulfilled in parallel, a delay is fulfilled when force is called upon it.

Futures, speculations, and delays are thus types of promises, and they only differ in how they are fulfilled. In fact they hardly differ in that regard since all must obey fulfill which, if successful, overrides any “fulfillment plan” that may be in place.

```lisp
(let ((f (future (+ 1 2)))
      (g (delay  (+ 3 4)))
      (h (delay  (+ 5 6))))
  (fulfill f 'nevermind)       ; may or may not cancel f's computation
  (fulfill g (+ 7 8))          ; 'force' will no longer compute (+ 3 4)
  (mapcar 'force (list f g h)))

; => (3 15 11) or (NEVERMIND 15 11)
```

f‘s planned computation is canceled if the first fulfill happens to grab the future before a worker thread gets it.

For an object which is not a promise, force behaves like identity, returning the object passed. We may imagine that non-promise objects are like promises that are always fulfilled. fulfilledp returns true for any non-promise argument passed. Attempting to fulfill a non-promise is not an error, though of course it never succeeds.

Lastly there is chain, which links objects together by relaying force and fulfilledp calls.

```lisp
(force (future (delay 3)))         ; => a delay object
(force (future (chain (delay 3)))) ; => 3
```

Suppose we wish to cancel a speculation and also signal an error if the speculation is forced after being canceled. This may be accomplished by giving a chained delay to fulfill.

```lisp
(let ((f (speculate (+ 3 4))))
  (fulfill f (chain (delay (error "speculation canceled!"))))
  (force f))

; => 7 or #<SIMPLE-ERROR "speculation canceled!">
```

If chain were not present then force would return a delay object if fulfill succeeded, on which force would have to be called again in order to obtain the error.
