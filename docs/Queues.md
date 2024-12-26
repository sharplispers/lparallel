Queues

The following symbols are exported by the lparallel.queue package. They are not included in the lparallel package.

[function]
make-queue &key fixed-capacity initial-contents

Create a queue.

The queue contents may be initialized with the keyword argument `initial-contents’.

By default there is no limit on the queue capacity. Passing a `fixed-capacity’ keyword argument limits the capacity to the value passed. `push-queue’ will block for a full fixed-capacity queue.

[function]
peek-queue queue

If `queue’ is non-empty, return (values element t) where `element’ is the frontmost element of `queue’.

If `queue’ is empty, return (values nil nil).

[function]
pop-queue queue

Remove the frontmost element from `queue’ and return it.

If `queue’ is empty, block until an element is available.

[function]
push-queue object queue

Push `object’ onto the back of `queue’.

[function]
queue-count queue

Return the number of elements in `queue’.

[function]
queue-empty-p queue

Return true if `queue’ is empty, otherwise return false.

[function]
queue-full-p queue

Return true if `queue’ is full, otherwise return false.

[function]
try-pop-queue queue &key timeout

If `queue’ is non-empty, remove the frontmost element from `queue’ and return (values element t) where `element’ is the element removed.

If `queue’ is empty and `timeout’ is given, then wait up to `timeout’ seconds for the queue to become non-empty.

If `queue’ is empty and the timeout has expired, or if `queue’ is empty and no `timeout’ was given, return (values nil nil).

Providing a nil or non-positive value of `timeout’ is equivalent to providing no timeout.

[macro]
with-locked-queue queue &body body

Execute `body’ with the queue lock held. Use the `/no-lock’ functions inside `body’.

