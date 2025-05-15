## *defpun*

See [defpun](./defpun.md)
for an introduction.

The following symbols are exported by the `lparallel.defpun` package as
well as by the convenience package `lparallel` .

------------------------------------------------------------------------

\[macro\] **declaim-defpun** `&rest` *`names`*

See \`defpun’.

\[macro\] **defpun** *`name`* *`params`* `&body` *`body`*

\`defpun’ defines a function which is specially geared for
fine-grained parallelism. If you have many small tasks which bog down
the system, \`defpun’ may help.

The syntax of \`defpun’ matches that of \`defun’. The difference is
that \`plet’ and \`plet-if’ take on new meaning inside \`defpun’.
The symbols in the binding positions of \`plet’ and \`plet-if’
should be viewed as lazily evaluated immutable references.

Inside a \`defpun’ form the name of the function being defined is a
macrolet, as are the names of other functions which were defined by
\`defpun’. Thus using \#’ on them is an error. Calls to functions
defined by \`defpun’ entail more overhead when the caller lies outside
a \`defpun’ form.

A \`defpun’ function must exist before it is referenced inside another
\`defpun’ function. If this is not possible–for example if func1 and
func2 reference each other–then use \`declaim-defpun’ to specify
intent:

(declaim-defpun func1 func2)

\[macro\] **defpun/type** *`name`* *`params`* *`arg-types`*
*`return-type`* `&body` *`body`*

Typed version of \`defpun’.

\`arg-types’ is an unevaluated list of argument types.

\`return-type’ is an unevaluated form of the return type, possibly
indicating multiple values as in (values fixnum float).

(As a technical point, if \`return-type’ contains no lambda list
keywords then the return type given to ftype will be additionally
constrained to match the number of return values specified.)

\[macro\] **plet** *`bindings`* `&body` *`body`*

The syntax of \`plet’ matches that of \`let’.

plet ({var-no-init \| (var \[init-form\])}\*) declaration\* form\*

For each (var init-form) pair, a future is created which executes
\`init-form’. Inside \`body’, \`var’ is a symbol macro which
expands to a \`force’ form for the corresponding future.

Each \`var-no-init’ is bound to nil and each \`var’ without
\`init-form’ is bound to nil (no future is created).

Type declarations for vars are recognized by \`plet’ and incorporated
into the final expansion. The semantics of these declarations are the
same as those of a regular \`let’ form.

\`plet’ is subject to optimization inside \`defpun’.

\[macro\] **plet-if** *`predicate`* *`bindings`* `&body` *`body`*

The syntax of \`plet-if’ matches that of \`let’ except for the
addition of the \`predicate’ form.

If \`predicate’ evaluates to true, the behavior is the same as
\`plet’.

If \`predicate’ evaluates to false, the behavior is the same as
\`let’.

\`plet-if’ is subject to optimization inside \`defpun’.

