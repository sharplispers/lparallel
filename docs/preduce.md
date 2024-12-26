preduce

preduce function sequence &key key from-end start end initial-value parts recurse

preduce (pronounced pee-reduce) is a parallel version of reduce. It chops up the input sequence into N parts and, in parallel, calls reduce on each part. The N partial results are then reduced again, either by reduce (the default) or, if the :recurse argument is non-nil, by preduce. The default value of N is the number of worker threads (the number given to make-kernel) which may be overridden by the :parts option.

(defpackage :example (:use :cl :lparallel))
(in-package :example)

(preduce '+ #(1 2 3 4 5 6) :parts 2)

is hand-wavingly similar to

(reduce '+ (vector (reduce '+ #(1 2 3))
                   (reduce '+ #(4 5 6))))

This code is misleading, of course: the two inner reduces are done in parallel, and the two inner arrays are displaced versions of the input array (no copying is done).

In order for the outcome of preduce to be independent of the choice of parts, the function passed must be associative with respect to the sequence elements and must produce an identity-like function when the :initial-value argument (if given) is partially applied. The latter condition is a consequence of :initial-value really meaning initial value per part.

(preduce '+ #(1 2 3 4 5 6) :parts 1 :initial-value 1)  ; => 22
(preduce '+ #(1 2 3 4 5 6) :parts 2 :initial-value 1)  ; => 23
(preduce '+ #(1 2 3 4 5 6) :parts 3 :initial-value 1)  ; => 24

In similar fashion, the :from-end option means from the end of each part.

The :start and :end arguments remain as they are in reduce, referring to the original input sequence.

The :key argument is thrown out while reducing the partial results. It applies to the first pass only.

preduce-partial is a variant of preduce which returns the unmolested partial results.

(preduce-partial '+ #(1 2 3 4 5 6) :parts 2)  ; => #(6 15)
(preduce-partial '+ #(1 2 3 4 5 6) :parts 3)  ; => #(3 7 11)

We can use preduce-partial to write premove-if for lists.

(defun premove-if* (test list)
  (reduce 'nreconc
          (preduce-partial (lambda (acc x)
                             (if (funcall test x)
                                 acc
                                 (cons x acc)))
                           list
                           :initial-value nil)
          :initial-value nil
          :from-end t))

It works as follows: after the partial results are returned by preduce-partial, each being a list in the reverse order of what we wish, we then walk backwards through the results, reversing and concatenating as we go.
