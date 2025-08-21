## Cognates

See [Cognates](./Cognates.md)
for an introduction.

The following symbols are exported by the `lparallel.cognate` package as
well as by the convenience package `lparallel` .

------------------------------------------------------------------------

\[macro\] **pand** `&rest` *`forms`*

Parallel version of \`and’. Forms in \`forms’ may be executed in
parallel, though not necessarily at the same time. If all forms evaluate
to true, then the result of any form may be returned.

\[function\] **pcount** *`item`* *`sequence`* `&key` *`from-end`*
*`start`* *`end`* *`key`* *`test`* *`test-not`* *`parts`*

Parallel version of \`count’.

The \`parts’ option divides \`sequence’ into \`parts’ number of
parts. Default is (kernel-worker-count).

\[function\] **pcount-if** *`predicate`* *`sequence`* `&key`
*`from-end`* *`start`* *`end`* *`key`* *`parts`*

Parallel version of \`count-if’.

The \`parts’ option divides \`sequence’ into \`parts’ number of
parts. Default is (kernel-worker-count).

\[function\] **pcount-if-not** *`predicate`* *`sequence`* `&rest`
*`args`* `&key` *`from-end`* *`start`* *`end`* *`key`* *`parts`*

Parallel version of \`count-if-not’.

The \`parts’ option divides \`sequence’ into \`parts’ number of
parts. Default is (kernel-worker-count).

\[macro\] **pdotimes** *`(var count &optional result parts)`* `&body`
*`body`*

Parallel version of \`dotimes’.

The \`parts’ option divides the integer range into \`parts’ number
of parts. Default is (kernel-worker-count).

Unlike \`dotimes’, \`pdotimes’ does not define an implicit block
named nil.

\[function\] **pevery** *`predicate`* `&rest` *`sequences`*

Parallel version of \`every’. Calls to \`predicate’ are done in
parallel, though not necessarily at the same time. Behavior is otherwise
indistinguishable from \`every’.

Keyword arguments \`parts’ and \`size’ are also accepted (see
\`pmap’).

\[function\] **pfind** *`item`* *`sequence`* `&rest` *`args`* `&key`
*`from-end`* *`test`* *`test-not`* *`start`* *`end`* *`key`* *`parts`*

Parallel version of \`pfind’.

The \`parts’ option divides \`sequence’ into \`parts’ number of
parts. Default is (kernel-worker-count).

\[function\] **pfind-if** *`predicate`* *`sequence`* `&rest` *`args`*
`&key` *`from-end`* *`start`* *`end`* *`key`* *`parts`*

Parallel version of \`pfind-if’.

The \`parts’ option divides \`sequence’ into \`parts’ number of
parts. Default is (kernel-worker-count).

\[function\] **pfind-if-not** *`predicate`* *`sequence`* `&rest`
*`args`* `&key` *`from-end`* *`start`* *`end`* *`key`* *`parts`*

Parallel version of \`pfind-if-not’.

The \`parts’ option divides \`sequence’ into \`parts’ number of
parts. Default is (kernel-worker-count).

\[macro\] **pfuncall** *`function`* `&rest` *`args`*

Parallel version of \`funcall’. Arguments in \`args’ may be executed
in parallel, though not necessarily at the same time.

\[macro\] **plet** *`bindings`* `&body` *`body`*

The syntax of \`plet’ matches that of \`let’.

plet ({var-no-init \| (var \[init-form\]) \| ((var1 var2 …)
\[init-form\])}\*) declaration\* form\*

For each (var init-form) pair, a future is created which executes
\`init-form’. Inside \`body’, \`var’ is a symbol macro which
expands to a \`force’ form for the corresponding future.

Likewise, each ((var1 var2 …) init-form) pair creates a future where
\`var1′, \`var2′,… are bound to the respective multiple return
values of \`init-form’.

Each \`var-no-init’ is bound to nil and each variable without a
corresponding \`init-form’ is bound to nil (no future is created).

Type declarations for vars are recognized by \`plet’ and incorporated
into the final expansion. The semantics of these declarations are the
same as those of a regular \`let’ form.

\`plet’ is subject to optimization inside \`defpun’.

\[macro\] **plet-if** *`predicate`* *`bindings`* `&body` *`body`*

The syntax of \`plet-if’ matches that of \`plet’ except for the
addition of the \`predicate’ form.

If \`predicate’ evaluates to true, the behavior is the same as
\`plet’.

If \`predicate’ evaluates to false, the behavior is the same as
\`slet’.

\`plet-if’ is subject to optimization inside \`defpun’.

\[function\] **pmap** *`result-type`* *`function`* `&rest` *`sequences`*

Parallel version of \`map’. Keyword arguments \`parts’ and \`size’
are also accepted.

The \`parts’ option divides each sequence into \`parts’ number of
parts. Default is (kernel-worker-count).

The \`size’ option limits the number of elements mapped to \`size’.
When given, no \`length’ calls are made on the sequence(s) passed.

Warning: \`size’ must be less than or equal to the length of the
smallest sequence passed. It is unspecified what happens when that
condition is not met.

\[function\] **pmap-into** *`result-sequence`* *`function`* `&rest`
*`sequences`*

Parallel version of \`map-into’. Keyword arguments \`parts’ and
\`size’ are also accepted (see \`pmap’).

\[function\] **pmap-reduce** *`map-function`* *`reduce-function`*
*`sequence`* `&rest` *`args`* `&key` *`start`* *`end`* *`initial-value`*
*`parts`* *`recurse`*

Equivalent to (preduce reduce-function sequence :key map-function …).

\[function\] **pmapc** *`function`* `&rest` *`lists`*

Parallel version of \`mapc’. Keyword arguments \`parts’ and
\`size’ are also accepted (see \`pmap’).

\[function\] **pmapcan** *`function`* `&rest` *`lists`*

Parallel version of \`mapcan’. Keyword arguments \`parts’ and
\`size’ are also accepted (see \`pmap’).

\[function\] **pmapcar** *`function`* `&rest` *`sequences`*

Parallel version of \`mapcar’. Keyword arguments \`parts’ and
\`size’ are also accepted (see \`pmap’).

Unlike \`mapcar’, \`pmapcar’ also accepts vectors.

\[function\] **pmapcon** *`function`* `&rest` *`lists`*

Parallel version of \`mapcon’. Keyword arguments \`parts’ and
\`size’ are also accepted (see \`pmap’).

\[function\] **pmapl** *`function`* `&rest` *`lists`*

Parallel version of \`mapl’. Keyword arguments \`parts’ and
\`size’ are also accepted (see \`pmap’).

\[function\] **pmaplist** *`function`* `&rest` *`lists`*

Parallel version of \`maplist’. Keyword arguments \`parts’ and
\`size’ are also accepted (see \`pmap’).

\[function\] **pmaplist-into** *`result-list`* *`function`* `&rest`
*`lists`*

Like \`pmaplist’ but results are stored in \`result-list’. Keyword
arguments \`parts’ and \`size’ are also accepted (see \`pmap’).

\[function\] **pnotany** *`predicate`* `&rest` *`sequences`*

Parallel version of \`notany’. Calls to \`predicate’ are done in
parallel, though not necessarily at the same time. Behavior is otherwise
indistinguishable from \`notany’.

Keyword arguments \`parts’ and \`size’ are also accepted (see
\`pmap’).

\[function\] **pnotevery** *`predicate`* `&rest` *`sequences`*

Parallel version of \`notevery’. Calls to \`predicate’ are done in
parallel, though not necessarily at the same time. Behavior is otherwise
indistinguishable from \`notevery’.

Keyword arguments \`parts’ and \`size’ are also accepted (see
\`pmap’).

\[macro\] **por** `&rest` *`forms`*

Parallel version of \`or’. Forms in \`forms’ may be executed in
parallel, though not necessarily at the same time. Any form which
evaluates to non-nil may be returned.

\[function\] **preduce** *`function`* *`sequence`* `&rest` *`args`*
`&key` *`key`* *`from-end`* *`start`* *`end`* *`initial-value`*
*`parts`* *`recurse`*

Parallel version of \`reduce’.

\`preduce’ subdivides the input sequence into \`parts’ number of
parts and, in parallel, calls \`reduce’ on each part. The partial
results are then reduced again, either by \`reduce’ (the default) or,
if \`recurse’ is non-nil, by \`preduce’.

\`parts’ defaults to (kernel-worker-count).

\`key’ is thrown out while reducing the partial results. It applies to
the first pass only.

\`start’ and \`end’ have the same meaning as in \`reduce’.

\`from-end’ means “from the end of each part”.

\`initial-value’ means “initial value of each part”.

\[function\] **preduce-partial** *`function`* *`sequence`* `&rest`
*`args`* `&key` *`key`* *`from-end`* *`start`* *`end`* *`initial-value`*
*`parts`*

Like \`preduce’ but only does a single reducing pass.

The length of \`sequence’ must not be zero.

Returns the partial results as a vector.

\[function\] **premove** *`item`* *`sequence`* `&rest` *`args`* `&key`
*`test`* *`test-not`* *`from-end`* *`start`* *`end`* *`key`* *`parts`*

Parallel version of \`remove’. Note the \`count’ option is not
supported.

The \`parts’ option divides \`sequence’ into \`parts’ number of
parts. Default is (kernel-worker-count).

\[function\] **premove-if** *`test`* *`sequence`* `&rest` *`args`*
`&key` *`from-end`* *`start`* *`end`* *`key`* *`parts`*

Parallel version of \`remove-if’. Note the \`count’ option is not
supported.

The \`parts’ option divides \`sequence’ into \`parts’ number of
parts. Default is (kernel-worker-count).

\[function\] **premove-if-not** *`test`* *`sequence`* `&rest` *`args`*
`&key` *`from-end`* *`start`* *`end`* *`key`* *`parts`*

Parallel version of \`remove-if-not’. Note the \`count’ option is
not supported.

The \`parts’ option divides \`sequence’ into \`parts’ number of
parts. Default is (kernel-worker-count).

\[function\] **psome** *`predicate`* `&rest` *`sequences`*

Parallel version of \`some’. Calls to \`predicate’ are done in
parallel, though not necessarily at the same time. Behavior is otherwise
indistinguishable from \`some’ except that any non-nil predicate
comparison result may be returned.

Keyword arguments \`parts’ and \`size’ are also accepted (see
\`pmap’).

\[function\] **psort** *`sequence`* *`predicate`* `&key` *`key`*
*`granularity`* `&allow-other-keys`

Parallel version of \`sort’.

If \`granularity’ is provided then parallel tasks are created only for
segments larger than \`granularity’. This may or may not result in
better performance.

At present \`psort’ is only parallelized for vectors; other types are
given to \`cl:sort’.

\[macro\] **slet** *`bindings`* `&body` *`body`*

\`slet’ (serial let) is the non-parallel counterpart to \`plet’.

The syntax of \`slet’ matches that of \`plet’, which includes the
ability to bind multiple values.

