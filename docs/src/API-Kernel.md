## Kernel

\[special variable\] **\*debug-tasks-p\***

If true (the default), the debugger is invoked when an error goes
unhandled inside a task, i.e. when the handlers established by
\`task-handler-bind’ (if any) do not handle it.

If false, unhandled errors from tasks are automatically transferred to
their parent thread (and/or any dependent threads) via the
\`transfer-error’ restart. This is for convenience — sometimes you
wish to avoid N debugger popups arising from N errors in N worker
threads.

For local control over debugger invocation, bind a task handler:

(task-handler-bind ((error \#’invoke-debugger)) …)

(task-handler-bind ((error \#’invoke-transfer-error)) …)

\[special variable\] **\*kernel\***

The current kernel, or nil.

\[special variable\] **\*kernel-spin-count\***

Default value of the \`spin-count’ argument to \`make-kernel’.

\[special variable\] **\*task-category\***

See \`kill-tasks’. Default value is \`:default’.

\[special variable\] **\*task-priority\***

When bound to \`:low’, the kernel schedules submitted tasks at low
priority. Default value is \`:default’.

\[function\] **broadcast-task** *`function`* `&rest` *`args`*

Wait for current and pending tasks to complete, if any, then
simultaneously execute the given task inside each worker. Wait until
these tasks finish, then return the results in a vector.

Calling \`broadcast-task’ from inside a worker is an error.

\[function\] **cancel-timeout** *`timeout`* *`timeout-result`*

Attempt to cancel a timeout. If successful, the channel passed to
\`submit-timeout’ will receive \`timeout-result’.

At most one call to \`cancel-timeout’ will succeed; others will be
ignored. If the timeout has expired on its own then \`cancel-timeout’
will have no effect.

\`cancel-timeout’ is not available in ABCL.

\`submit-timeout’ and \`cancel-timeout’ are deprecated; use the new
\`:timeout’ option in \`try-receive-result’.

\[function\] **check-kernel**

Ensures the value of \`\*kernel\*’ is a kernel instance. Provides
the MAKE-KERNEL and STORE-VALUE restarts. Returns \`\*kernel\*’.

\[function\] **end-kernel** `&key` *`wait`*

Sets \`\*kernel\*’ to nil and ends all workers gracefully.

\`end-kernel’ should not be used as a substitute for properly waiting
on tasks with \`receive-result’ or otherwise.

If \`wait’ is nil (the default) then \`end-kernel’ returns
immediately. Workers are waited upon by a separate shutdown manager
thread.

If \`wait’ is non-nil then \`end-kernel’ blocks until all workers
are finished. No shutdown manager thread is created.

A list of the implementation-defined worker thread objects is returned.
If \`wait’ is nil then the shutdown manager thread is also returned as
the first element in the list.

Note that creating and destroying kernels is relatively expensive. A
kernel typically exists for lifetime of the Lisp process. Having more
than one kernel is fine — simply use \`let’ to bind a kernel instance
to \`\*kernel\*’ when you need it. Use \`kill-tasks’ to
terminate deadlocked or infinite looping tasks.

\[function\] **invoke-transfer-error** *`error`*

Equivalent to (invoke-restart ‘transfer-error error).

This is a convenience function for use in \`task-handler-bind’.

\[function\] **kernel-bindings**

Return the bindings passed to \`make-kernel’.

\[function\] **kernel-context**

Return the context passed to \`make-kernel’.

\[function\] **kernel-name**

Return the name passed to \`make-kernel’.

\[function\] **kernel-worker-count**

Return the number of workers in the current kernel.

\[function\] **kernel-worker-index**

If called from inside a worker, return the worker’s assigned index,
ranging from 0 to one less than (kernel-worker-count).

If not called from inside a worker, return nil.

\[function\] **kill-tasks** *`task-category`* `&key` *`dry-run`*

This is an expensive function which should only be used in exceptional
circumstances.

Every task has an associated task category. When a task is submitted, it
is assigned the category of \`\*task-category\*’ which has a
default value of \`:default’.

\`kill-tasks’ interrupts running tasks whose category is \`eql’ to
\`task-category’. The corresponding worker threads are killed and
replaced. Pending tasks are not affected.

If you don’t know what to pass for \`task-category’ then you should
probably pass \`:default’, though this may kill more tasks than you
wish. Binding \`\*task-category\*’ around \`submit-task’ enables
targeted task killing.

If \`dry-run’ is nil, the function returns the number of tasks killed.

If \`dry-run’ is non-nil then no tasks are killed. In this case the
return value is the number of tasks that would have been killed if
\`dry-run’ were nil.

\`kill-tasks’ is not available in ABCL.

\[function\] **make-channel** `&rest` *`args`*

Create a channel for submitting and receiving tasks. The current value
of \`\*kernel\*’ is stored for use in \`submit-task’.

By default there is no limit on the channel capacity. Passing a
\`fixed-capacity’ keyword argument limits the capacity to the value
passed.

Note that a fixed capacity channel may cause a deadlocked kernel if
\`receive-result’ is not called a sufficient number of times.

\[function\] **make-kernel** *`worker-count`* `&key` *`name`*
*`bindings`* *`context`* *`spin-count`* *`use-caller`*

Create a kernel with \`worker-count’ number of worker threads.

\`name’ is a string identifier for this kernel which is reported by
\`print-object’. Worker threads will also be given this name, shown in
\`bordeaux-threads:all-threads’.

\`bindings’ is an alist for establishing thread-local variables inside
worker threads. By default workers will have \*standard-output\* and
\*error-output\* bindings.

Dynamic context for each worker may be established with the function
\`context’. The argument passed to \`context’ is a function which
must be funcalled. It begins the worker loop and will not return until
the worker exits. The default value of \`context’ is \#’funcall. The
special variables in \`bindings’ are available inside the \`context’
function.

When a worker discovers that no tasks are available, \`spin-count’ is
the number of task-searching iterations done by the worker before
sleeping.

If \`use-caller’ is true (default is false) then in certain situations
the calling thread may be enlisted to steal work from worker threads.
This is an optimization strategy that currently applies only during the
execution of functions defined by \`defpun’ and \`defpun/type’.
Typically in this case the number of workers will be one less than the
number of cores/CPUs.

A kernel will not be garbage collected until \`end-kernel’ is called.

\[function\] **receive-result** *`channel`*

Remove a result from \`channel’. If nothing is available the call will
block until a result is received.

\[function\] **submit-task** *`channel`* *`function`* `&rest` *`args`*

Submit a task through \`channel’ to the kernel stored in \`channel’.

\[function\] **submit-timeout** *`channel`* *`timeout-seconds`*
*`timeout-result`*

Effectively equivalent to

(submit-task channel (lambda () (sleep timeout-seconds) timeout-result))

The difference is that \`submit-timeout’ does not occupy a worker
thread.

A timeout object is returned, which may be passed to
\`cancel-timeout’.

\`submit-timeout’ and \`cancel-timeout’ are deprecated; use the new
\`:timeout’ option in \`try-receive-result’.

\[function\] **task-categories-running**

Return a vector containing the task category currently running for each
worker.

\[macro\] **task-handler-bind** *`clauses`* `&body` *`body`*

Like \`handler-bind’ but handles conditions signaled inside tasks that
were created in \`body’.

\[function\] **try-receive-result** *`channel`* `&key` *`timeout`*

If \`channel’ has a result then remove it and return (values result
t).

If no result is available and \`timeout’ is given, then wait up to
\`timeout’ seconds for a result.

If the channel is empty and the timeout has expired, or if the channel
is empty and no timeout was given, return (values nil nil).

Providing a nil or non-positive value of \`timeout’ is equivalent to
providing no timeout.

