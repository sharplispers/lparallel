
lparallel-2.8.0 released
Posted on May 8, 2015 by lparallel	

    added broadcast-task — executes a given task inside each worker
    added kernel-worker-index — determine if the current thread is a
    worker thread, and if so, obtain its assigned index 

Posted in Uncategorized	| Leave a comment
lparallel-2.7.0 released
Posted on April 10, 2014 by lparallel	

    added the ability to bind multiple values in plet, e.g.
    (plet ((x 1) ((y z) (values 2 3))) (+ x y z)) ;=> 6
    added slet — serial/non-parallel let with the same syntax as plet for binding multiple values
    various optimizations

Posted in Uncategorized	| Leave a comment
lparallel-2.6.0 released
Posted on October 27, 2013 by lparallel	

    added a :timeout option to try-pop-queue and try-receive-result; this requires the latest version of bordeaux-threads (lparallel will still run with older versions of bordeaux-threads as long as the :timeout option is not used)
    optimizations to defpun
    optimizations from smaller generated code size

Posted in Uncategorized	| Leave a comment
Introducing lfarm
Posted on June 15, 2013 by lparallel	

lfarm is a distributed version of lparallel which replaces worker threads with remote processes. For example lfarm:pmap will subdivide the input sequence(s), send the parts to remote machines for mapping, and then combine the results. Likewise lfarm:future wraps remote task execution in the metaphor of promises. Most of the lparallel kernel API is retained with minor variations.
Posted in Uncategorized	| Leave a comment
lparallel-2.4.0 released
Posted on May 30, 2013 by lparallel	

    plet now exploits type declarations
    defpun*, defpun/type*, and psort* are now deprecated — instead use the unstarred versions and pass :use-caller t to make-kernel
    parallel compilation is now safe

Posted in Uncategorized	| Leave a comment
lparallel-2.3.0 released
Posted on February 2, 2013 by lparallel	
make-queue and make-channel now accept a :fixed-capacity argument for limiting the number of elements stored
make-queue now accepts an :initial-contents argument
passing a single argument to make-queue or make-channel is deprecated; a &rest hack is present for backward compatibility
added function queue-full-p
Posted in Uncategorized	| Leave a comment
lparallel-2.2.0 released
Posted on January 5, 2013 by lparallel	

    exported types: kernel, channel, ptree
    added ptree-computed-p — query the computed state of a ptree node
    make-kernel now aborts cleanly when a worker fails to initialize, e.g. when make-thread fails or when a :context function aborts
    check-kernel now returns a kernel instance
    added a front-end lock to some ptree functions — removes the requirement that some calls be exclusive
    improved performance of functions defined by defpun

Posted in Uncategorized	| Leave a comment
lparallel-2.1.0 released
Posted on November 11, 2012 by lparallel	

    added readers kernel-name and kernel-context
    added restart kill-errors to workers — removes debugger popups
    attempting to submit a task to an ended kernel now signals an error
    suicidal calls to kill-tasks inside workers are now permitted

Posted in Uncategorized	| Leave a comment
Concurrent Hello World
Posted on November 5, 2012 by lparallel	

Below is another example of Concurrent Hello World. The macros receive and run are just 9 lines each, given below the fold.

(defpackage :example (:use :cl :node))
(in-package :example)

(defun hello (hello-queue world-queue)
  (receive hello-queue
    (:say-hello (n)
      (cond ((plusp n)
             (format t "Hello ")
             (send world-queue :say-world n))
            (t
             (send world-queue :quit)
             (return))))))

(defun world (world-queue hello-queue)
  (receive world-queue
    (:say-world (n)
      (format t "World!~%")
      (send hello-queue :say-hello (- n 1)))
    (:quit ()
      (return))))

(defun main (n)
  (let ((hello-queue (make-queue))
        (world-queue (make-queue)))
    (run
      (make-node 'hello hello-queue world-queue)
      (make-node 'world world-queue hello-queue)
      (send hello-queue :say-hello n))))

Continue reading →
Posted in Uncategorized	| 5 Comments
lparallel-2.0.0 released
Posted on November 3, 2012 by lparallel	

The major version bump is for some incompatible changes, though they are unlikely to cause trouble.

    keyword arguments to psort besides :key have been replaced with a single :granularity argument; the old arguments are now ignored
    removed deprecated aliases from 1.2.0 and 1.3.0 (you may not be aware of them since they haven’t been listed in the documentation)
    A function defined with defpun is now optimized for N worker threads where N is the number of cores. The old behavior is available with defpun*, which defines a function that is optimized for N-1 workers (and has less overhead).
    added psort* — like psort but targets N-1 workers
    improved performance of psort
    task categories are now compared with eql; same for ptree node ids

The benchmarks page has been updated to reflect recent optimizations, which includes optimizations in SBCL itself.


Status update
Posted on October 8, 2012 by lparallel	

    The latest CLISP and ECL appear close to passing all tests, and may indeed pass on some platforms.

    I am not aware of any outstanding CL implementation bugs affecting lparallel besides those reported in CLISP and ECL. I am also not aware of any bugs in lparallel itself.

    If you happen to be using bignums on 32-bit x86 CCL 1.8 or earlier, you should get latest from the CCL repository and build a new image.

    lparallel does not seem complex enough to warrant a mailing list, yet some things may not be entirely simple either. Feel free to ask questions or offer feedback on this thread, or send me email.

    I plan to remove some old deprecated aliases in version 2.0 (they are not shown in the documentation here). Now is the time to suggest incompatible changes, before the 2.0 bump.

    I have been hesitant to add a nickname to the lparallel package. The separation of the lparallel API into a handful of packages was meant to encourage people to use a subset of the API, e.g. (:use :lparallel.cognate). However some people always write package-qualified symbols, and for them an lp or ll nickname would be convenient. I am not exactly against this, but it does entail a bit of peril in the form of increased likelihood of conflict.

    I have noticed this pattern being used: (let ((*kernel* (make-kernel ...))) ...). This is not recommended for three reasons. First, it makes the kernel object inaccessible to other (non-worker) threads, preventing the use of kill-tasks in the REPL for example. Second, end-kernel is likely to be forgotten, resulting in a kernel that is not garbage collected. Third, even if we properly abstract this pattern by writing a with-temp-kernel macro that calls end-kernel, such a macro lends itself to suboptimal code because multiple uses of it would defeat the benefits of a thread pool. These issues are avoided by calling (setf *kernel* ...) or by binding to an existing kernel, for example (let ((*kernel* *io-kernel*)) ...).

    A with-temp-kernel macro may still be convenient in non-critical cases such as testing, yet I hesitate to include it in the lparallel API for the reasons mentioned above. 

Posted in Uncategorized	| Leave a comment
lparallel-1.7.0 released
Posted on October 1, 2012 by lparallel	

    added pdotimes
    optimized cognate functions and macros when they are called inside worker threads; e.g. pmap in (future (pmap ...)) no longer blocks a worker

Posted in Uncategorized	| Leave a comment
lparallel-1.6.0 released
Posted on August 1, 2012 by lparallel	

    added clear-ptree-errors — for resuming after an error
    added clear-ptree — for recomputing from scratch
    improved task handling for ptrees
    :lparallel now in *features* after load
    defpun no longer transforms pfuncall forms

Posted in Uncategorized	| Leave a comment
Auto-kill
Posted on June 24, 2012 by lparallel	

When an evaluation fails or is interrupted, it may be convenient to automatically kill tasks created during the evaluation. One use for this might be for debugging a set of long-running tasks. Here is a solution using alexandria’s unwind-protect-case.

(defpackage :example (:use :cl :lparallel :alexandria))
(in-package :example)

(defun call-with-kill-on-abort (fn task-category)
  (let ((*task-category* task-category))
    (unwind-protect-case ()
        (funcall fn)
      (:abort (kill-tasks task-category)))))

(defmacro with-kill-on-abort ((&key (task-category '*task-category*))
                              &body body)
  `(call-with-kill-on-abort (lambda () ,@body) ,task-category))

(defun foo ()
  (with-kill-on-abort (:task-category 'foo-stuff)
    (pmap nil #'sleep '(9999 9999))))

Example run in SLIME:

CL-USER> (example::foo) ; ... then hit C-c-c
WARNING: lparallel: Replacing lost or dead worker.
WARNING: lparallel: Replacing lost or dead worker.
; Evaluation aborted on NIL.

As always, worker threads are regenerated after being killed.
Posted in Uncategorized	| 1 Comment
Miscellany
Posted on June 10, 2012 by lparallel	
Mapping

It should be no surprise that arrays are faster than lists for parallel mapping. The open-coded versions of pmap and pmap-into, which are triggered when a single array is mapped to an array, are particularly fast in SBCL when the array types are declared or inferred. For the extreme case of a trivial inline function applied to a large array, the speed increase can be 20X or more relative to the non-open-coded counterparts.
Condition handling under the hood

To the user, a task is a function designator together with arguments to the function. However the internal representation of a task is like a generalization of a closure. A closure is a function which captures the lexical variables referenced inside it. Implementation-wise, a task is a closure which captures the task handlers present when the task is created. A closure bundles a lexical environment; a task additionally bundles a dynamic environment. This is the basic theory behind parallelized condition handling in lparallel.
Communicating via conditions

Because task handlers are called immediately when a condition is signaled inside a task, condition handling offers a way to communicate between tasks and the thread which created them. Here is a task which transfers data by signaling:

(defpackage :example (:use :cl :lparallel :lparallel.queue))
(in-package :example)

(define-condition more-data ()
  ((item :reader item :initarg :item)))

(let ((channel (make-channel))
      (data (make-queue)))
  (task-handler-bind ((more-data (lambda (c)
                                   (push-queue (item c) data))))
    (submit-task channel (lambda ()
                           (signal 'more-data :item 99))))
  (receive-result channel)
  (pop-queue data))

; => 99

receive-result has been placed outside of task-handler-bind to emphasize that handlers are bundled at the point of submit-task. (It doesn’t matter where receive-result is called.)
Multiple kernels

It may be advantageous to have a kernel devoted to particular kinds of tasks. For example one could have specialized channels and futures dedicated to IO.

(defvar *io-kernel* (make-kernel 16))

(defun make-io-channel ()
  (let ((*kernel* *io-kernel*))
    (make-channel)))

(defmacro io-future (&body body)
  `(let ((*kernel* *io-kernel*))
     (future ,@body)))

Since a channel remembers its associated kernel, submit-task and receive-result do not depend upon the value of *kernel*. In the promise API, only future and speculate need *kernel*.
Posted in Uncategorized	| Leave a comment
lparallel-1.5.0 released
Posted on June 2, 2012 by lparallel	

    pmap and pmap-into are now open-coded in the case of one vector being mapped to a vector — allows a large performance boost in some CL implementations (like SBCL) when array types are known
    SBCL is now able to terminate when live kernels exist — previously, end-kernel needed to be called on all kernels before exiting (which is good practice but is no longer required)
    added try-receive-result — non-blocking version of receive-result

Posted in Uncategorized	| Leave a comment
lparallel-1.4.0 released
Posted on May 14, 2012 by lparallel	

    added function task-categories-running
    new special variable *debug-tasks-p* — setting it to false will transfer errors instead of invoking the debugger inside tasks; default is true
    added convenience function invoke-transfer-error for local control over debugging tasks:
    (task-handler-bind ((error #'invoke-transfer-error)) ...)
    (task-handler-bind ((error #'invoke-debugger)) ...)

Posted in Uncategorized	| Leave a comment
lparallel-1.3.0 released
Posted on May 5, 2012 by lparallel	

    new support for fine-grained parallelism with `defpun’
    new work-stealing model with lockless queues and optional spinning; enabled by default on SBCL, others default to central queue
    added pfind, pcount, plet-if, pfuncall
    fixed redundant restart in `chain’
    `fulfill’ now accepts non-promises (never succeeds)
    removed high optimizations exposed in some API functions
    added shell script for unthrottling CPUs in Linux
    renamed *kernel-task-category* -> *task-category*, *kernel-task-priority* -> *task-priority*, kernel-handler-bind -> task-handler-bind, preduce/partial -> preduce-partial; old names are still available

Posted in Uncategorized	| Leave a comment
lparallel-1.2.0 released
Posted on March 23, 2012 by lparallel	

    added function cancel-timeout; submit-timeout now returns a timeout object
    renamed emergency-kill-tasks to kill-tasks; old name is still available
    minor optimization to ptrees
    added type checks to psort arguments
    switched test framework to eos

Posted in Uncategorized	| Leave a comment
lparallel-1.1.0 released
Posted on January 24, 2012 by lparallel	

    added :wait option to end-kernel — blocks until the kernel has shut down
    (please read the documentation for end-kernel before using)
    bound *print-circle* to t when printing a kernel — avoids SBCL + SLIME
    crash when evaluating the single form (setf *kernel* (make-kernel …))

Posted in Uncategorized	| Leave a comment 


lparallel is now part of Quicklisp
Posted on November 7, 2011 by lparallel	

(ql:quickload :lparallel)

Posted in Uncategorized	| Leave a comment
Welcome
Posted on October 22, 2011 by lparallel	

lparallel is a new library for parallelism in Common Lisp.

The buttons at the top link to everything you need to know about lparallel.
Posted in Uncategorized	| Leave a comment 
