## Handling

Handling conditions in lparallel is done with task-handler-bind. It is just like handler-bind except that it handles conditions signaled from inside parallel tasks.

```lisp
(defpackage :example (:use :cl :lparallel))
(in-package :example)

(define-condition foo-error (error) ())

(task-handler-bind ((foo-error (lambda (e)
                                 (declare (ignore e))
                                 (invoke-restart 'number-nine))))
  (pmapcar (lambda (x)
             (declare (ignore x))
             (restart-case (error 'foo-error)
               (number-nine () "number nine")))
           '(1 2 3)))

; => ("number nine" "number nine" "number nine")
```

Though one may be tempted to merge handler-bind and task-handler-bind with some shadowing magic, in general the handlers which need to reach inside tasks will not always match the handlers that are suitable for the current thread. It is also useful to explicitly flag asynchronous handlers that require thread-safe behavior.

In Common Lisp, the debugger is invoked when an error goes unhandled. By default lparallel mirrors this behavior with regard to tasks: when an error is signaled inside a task, and the error is not handled by one of the task handlers established by task-handler-bind, then the debugger is invoked.

However there is an alternate behavior which may be more appropriate depending upon the situation: automatically transferring errors. Setting `*debug-tasks-p*` to false will transfer task errors to threads which attempt to obtain the failed results. Suppose you have several parallel tasks running and each task signals an error. If `*debug-tasks-p*` is false then the debugger will be invoked just once (typically in the parent thread) instead of several times (once for each task).

If `*debug-tasks-p*` is true then you may still elect to transfer the error yourself. Inside each task there is a restart called TRANSFER-ERROR, which you will see in the debugger. (When `*debug-tasks-p*` is false the restart is simply invoked for you.) The following shows the Clozure + SLIME environment.

```lisp
(pmapcar (lambda (x)
           (when (evenp x)
             (restart-case (error 'foo-error)
               (number-nine ()
                 :report "Who was to know?"
                 "number nine"))))
         '(1 2 3))
=>
Error EXAMPLE::FOO-ERROR
   [Condition of type EXAMPLE::FOO-ERROR]

Restarts:
 0: [NUMBER-NINE] Who was to know?
 1: [TRANSFER-ERROR] Transfer this error to a dependent thread, if one exists
 2: [ABORT-BREAK] Reset this thread
 3: [ABORT] Kill this thread
```

The presence of the TRANSFER-ERROR restart indicates that we are inside a task. After inspecting the backtrace to our satisfaction, it’s time to hit TRANSFER-ERROR. In our example the top-level thread is already waiting for the result, so the debugger will appear again after we transfer.

```lisp
=>
Error FOO-ERROR
   [Condition of type FOO-ERROR]

Restarts:
 0: [RETRY] Retry SLIME interactive evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT-BREAK] Reset this thread
 3: [ABORT] Kill this thread
```

The familiar SLIME restarts are there again. We are back in the top-level thread.

The behavior specified by *debug-tasks-p* may be locally overridden with task-handler-bind. To always transfer errors,

```lisp
(task-handler-bind ((error #'invoke-transfer-error)) ...)
```

Likewise to always invoke the debugger for unhandled errors,

```lisp
(task-handler-bind ((error #'invoke-debugger)) ...)
```

More on threads

In the second example, what if we selected the ABORT restart (“Kill this thread”) instead of transferring the error? This would not be dangerous—the killed worker would be automatically replaced with a new one—but it would be a little rude. The top-level thread would signal TASK-KILLED-ERROR instead of FOO-ERROR. In our example this does not matter, but by signaling TASK-KILLED-ERROR we potentially spoil a handler’s lifelong dream of handling a FOO-ERROR.
Killing tasks

Occasionally there may be a task which has entered a deadlock (which can happen with circular references) or an infinite loop. Don’t panic! Try

```lisp
(kill-tasks :default)
```

This is a blunt weapon, however, because passing :default may cause unrelated tasks to be killed.

Each task is given a task category identifier. When a task is submitted, it is assigned the category of *task-category* which has a default value of :default. The argument to kill-tasks is a task category. Any running task whose category is eql to the argument passed will be killed. Pending tasks are not affected.

To avoid killing unrelated tasks, bind `*task-category*` around submit-task calls.

```lisp
(let ((channel (make-channel)))
  ;; ...
  (let ((*task-category* 'my-stuff))
    (submit-task channel (lambda () (loop))))  ; oops!
  (receive-result channel))
```

This is hung at receive-result. We can recover by calling

```lisp
(kill-tasks 'my-stuff)
```

which will only kill our looping task, assuming my-stuff is not used as a task category elsewhere in the same package.

Keep in mind that killing tasks is expensive and should only be done in exceptional circumstances. Not only is thread creation expensive (for the worker replacements), but heavy locking is required as well.
