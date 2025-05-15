## Kernel

In the context of lparallel, a kernel is an abstract entity that schedules and executes tasks. The lparallel kernel API is meant to describe parallelism in a generic manner.

The implementation uses a group of worker threads. It is intended to be efficiency-wise comparable to (or faster than) similar hand-rolled solutions while also providing full condition handling and consistency checks. All higher-level constructs in lparallel are implemented on top of the kernel.

(For an implementation of the kernel API that distributes across machines, see lfarm.)

Kernel-related operations are applied to the current kernel, stored in *kernel*. A kernel is typically created with

```lisp
(setf lparallel:*kernel* (lparallel:make-kernel N))
```

where N is the number of worker threads (more options are available). In most circumstances a kernel should exist for the lifetime of the Lisp process. Multiple kernels are possible, and setting the current kernel is done in the expected manner by dynamically binding *kernel*.

A task is a function designator together with arguments to the function. To execute a task, (1) create a channel, (2) submit the task through the channel, and (3) receive the result from the channel.

```lisp
(defpackage :example (:use :cl :lparallel))
(in-package :example)

(let ((channel (make-channel)))
  (submit-task channel '+ 3 4)
  (receive-result channel))

; => 7
```

If you have not created a kernel (if `*kernel*` is nil) then upon evaluating the above you will receive an error along with a restart offering to make a kernel for you. Evaluation commences once a kernel is created.

Multiple tasks may be submitted on the same channel, though the results are not necessarily received in the order in which they were submitted. receive-result receives one result per call.

```lisp
(let ((channel (make-channel)))
  (submit-task channel '+ 3 4)
  (submit-task channel (lambda () (+ 5 6)))
  (list (receive-result channel)
        (receive-result channel)))

; => (7 11) or (11 7)
```

To set the priority of tasks, bind *task-priority* around calls to submit-task.

```lisp
(let ((*task-priority* :low))
  (submit-task channel '+ 3 4))
```

The kernel executes a :low priority task only when there are no default priority tasks pending. The task priorities recognized are :default (the default priority) and :low.

Handlers may be established for conditions that are signaled by a task (see Handling). When an error from a task goes unhandled, an error-info object is placed in the channel. After receive-result removes such an object from the channel, the corresponding error is signaled.

Note that a kernel will not be garbage collected until end-kernel is called.
Message passing

For situations where submit-task and receive-result are too simplistic, a blocking queue is available for arbitrary message passing between threads.

```lisp
(defpackage :queue-example (:use :cl :lparallel :lparallel.queue))
(in-package :queue-example)

(let ((queue (make-queue))
      (channel (make-channel)))
  (submit-task channel (lambda () (list (pop-queue queue)
                                        (pop-queue queue))))
  (push-queue "hello" queue)
  (push-queue "world" queue)
  (receive-result channel))
;; => ("hello" "world")
```

Of course messages may also be passed between workers.
Dynamic variables and worker context

When a dynamic variable is dynamically bound (for example with let or progv), the binding becomes local to that thread. Otherwise, the global (default) value of a dynamic variable is shared among all threads that access it.

Binding dynamic variables for use inside tasks may be done on either a per-task basis or a per-worker basis. An example of the former is

```lisp
(submit-task channel (let ((foo *foo*))
                       (lambda ()
                         (let ((*foo* foo))
                           (bar)))))
```

This saves the current value of `*foo*` and, inside the task, binds `*foo*` to that value for the duration of (bar). You may wish to write a submit-with-my-bindings function to suit your particular needs.

To establish permanent dynamic bindings inside workers (thread-local variables), use the :bindings argument to make-kernel, which is an alist of (var-name . value-form) pairs. Each value-form is evaluated inside each worker when it is created. (So if you have two workers, each value-form will be evaluated twice.)

For more complex scenarios of establishing worker context, a :context function may be provided. This function is called by lparallel inside each worker and is responsible for entering the worker loop by funcalling its only parameter. The variables from :bindings are available inside the function.

```lisp
(defvar *foo* 0)
(defvar *bar* 1)

(defun my-worker-context (worker-loop)
  (let ((*bar* (1+ *foo*)))
    ;; enter the worker loop; return when the worker shuts down
    (funcall worker-loop)))

(defvar *my-kernel* (make-kernel 2
                                 :bindings '((*foo* . (1+ 98)))
                                 :context #'my-worker-context))

(list *foo* *bar*)
; => (0 1)

(let* ((*kernel* *my-kernel*)
       (channel (make-channel)))
  (submit-task channel (lambda () (list *foo* *bar*)))
  (receive-result channel))
; => (99 100)
```
