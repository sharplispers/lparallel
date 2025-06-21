## Promises

See [Promises](./Promises.md)
for an introduction.

The following symbols are exported by the `lparallel.promise` package as
well as by the convenience package `lparallel` .

------------------------------------------------------------------------

\[function\] **chain** *`object`*

Create a chain. A chain links objects together by relaying \`force’
and \`fulfilledp’ calls.

\[macro\] **delay** `&body` *`body`*

Create a delay. A delay is a promise which is fulfilled when \`force’
is called upon it.

\[function\] **force** *`object`*

If \`object’ is a promise and the promise is fulfilled, return the
fulfilled value (possibly multiple values). If the promise is
unfulfilled then the call blocks until the promise is fulfilled.

If \`object’ is a chain, call \`force’ on the chained object.

If \`object’ is not a promise and not a chain, return the identical
object passed.

Note if \`force’ is called on an unfulfilled future then the future is
fulfilled by the caller of \`force’.

\[macro\] **fulfill** *`object`* `&body` *`body`*

Attempt to give \`object’ a value.

If \`object’ is a promise which is not fulfilled and not currently
being fulfilled, then the implicit progn \`body’ will be executed and
the promise will store the result. In this case \`fulfill’ returns
true.

If \`object’ is a promise that is either already fulfilled or actively
being fulfilled, then \`body’ will not be executed and \`fulfill’
returns false.

If \`object’ is a chain, call \`fulfill’ on the chained object.

If \`object’ is not a promise and not a chain then false is returned
immediately, with \`body’ being ignored.

\[function\] **fulfilledp** *`object`*

If \`object’ is a promise, return a boolean indicating whether the
promise is fulfilled.

If \`object’ is a chain, call \`fulfilledp’ on the chained object.

If \`object’ is not a promise and not a chain, return true.

\[macro\] **future** `&body` *`body`*

Create a future. A future is a promise which is fulfilled in parallel by
the implicit progn \`body’.

\[function\] **promise**

Create a promise. A promise is a receptacle for a result which is
unknown at the time it is created.

\[macro\] **speculate** `&body` *`body`*

Create a speculation. A speculation is a low-priority future.

