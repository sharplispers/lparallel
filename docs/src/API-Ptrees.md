## Ptrees

See [Ptrees](./Ptrees.md)
for an introduction.

The following symbols are exported by the `lparallel.ptree` package as
well as by the convenience package `lparallel` .

------------------------------------------------------------------------

\[special variable\] **\*ptree-node-kernel\***

When non-nil, \`\*kernel\*’ is bound to this value during the call
of a node function.

\[function\] **call-ptree** *`id`* *`ptree`*

Return the computation result of the node with identifier \`id’ in
\`ptree’.

If the node is uncomputed, compute the result.

If the node is already computed, return the computed result.

\[function\] **check-ptree** *`ptree`*

Verify that all nodes have been defined with an associated function. If
not, \`ptree-undefined-function-error’ is signaled.

\[function\] **clear-ptree** *`ptree`*

Clear all node results in \`ptree’, restoring the tree to its
uncomputed state.

\[function\] **clear-ptree-errors** *`ptree`*

Clear all error results in \`ptree’, allowing the computation to
resume from its latest pre-error state.

\[function\] **make-ptree**

Create a ptree instance.

\[macro\] **ptree** *`defs`* `&body` *`body`*

Create a ptree using \`flet’ syntax.

ptree ((node-name child-names function-body)\*) form\*

Each \`node-name’ form corresponds to the definition of a ptree node.

\`node-name’ is the name of the node being defined (a symbol).

\`child-names’ is a list of the names of child nodes (symbols).

The function associated with the node being defined is

\`(lambda ,child-names ,@function-body)

\`child-names’ cannot contain lambda list keywords.

For each \`node-name’, a symbol macro is defined which expands to a
\`call-ptree’ form for that node.

\[function\] **ptree-computed-p** *`id`* *`ptree`*

Return true if the node with identifier \`id’ in \`ptree’ has
finished computing, otherwise return false.

\[function\] **ptree-fn** *`id`* *`args`* *`function`* *`ptree`*

Define a ptree node with identifier \`id’, which is some unique object
suitable for \`eql’ comparison such as symbol.

The ids of its child nodes are elements of the list \`args’.

\`function’ is the function associated with this node. The arguments
passed to \`function’ are the respective results of the child node
computations.

\`ptree’ is the ptree instance in which the node is being defined.

