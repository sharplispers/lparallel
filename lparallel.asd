;;; Copyright (c) 2011-2012, James M. Lawrence. All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;
;;;     * Redistributions in binary form must reproduce the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer in the documentation and/or other materials provided
;;;       with the distribution.
;;;
;;;     * Neither the name of the project nor the names of its
;;;       contributors may be used to endorse or promote products derived
;;;       from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; plet uses a cltl2 feature
  #+(or sbcl ccl lispworks allegro)
  (progn
    (pushnew :lparallel.with-cltl2 *features*)
    #+sbcl (require :sb-cltl2))

  ;; thread kill does not call unwind-protect cleanup forms
  #+abcl
  (pushnew :lparallel.without-kill *features*))

(defsystem :lparallel
  :version "2.8.4"
  :description "Parallelism for Common Lisp"
  :long-description
"
lparallel is a library for parallel programming in Common Lisp, featuring

  * a simple model of task submission with receiving queue
  * constructs for expressing fine-grained parallelism
  * asynchronous condition handling across thread boundaries
  * parallel versions of map, reduce, sort, remove, and many others
  * promises, futures, and delayed evaluation constructs
  * computation trees for parallelizing interconnected tasks
  * bounded and unbounded FIFO queues
  * high and low priority tasks
  * task killing by category
  * integrated timeouts

See http://lparallel.org for documentation and examples.
"
  :licence "BSD"
  :author "James M. Lawrence <llmjjmll@gmail.com>"
  :depends-on (:alexandria
               :bordeaux-threads)
  :serial t
  :components ((:module "src"
                :serial t
                :components
		((:module "util"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "config")
                   (:file "misc")
                   (:file "defmacro")
                   (:file "defun")
                   (:file "defslots")
                   (:file "defpair")))
                 (:file "thread-util")
                 (:file "raw-queue")
                 (:file "cons-queue")
                 (:file "vector-queue")
                 (:file "queue")
                 (:file "counter")
                 (:file "spin-queue")
                 (:module "kernel"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "specials")
                   (:file "handling")
                   (:file "classes")
                   (:file "stealing-scheduler")
                   (:file "kill" :if-feature (:not :lparallel.without-kill))
                   (:file "core")
                   (:file "timeout")))
                 (:file "kernel-util")
                 (:file "promise")
                 (:file "ptree")
                 (:file "slet")
                 (:file "defpun")
                 (:module "cognate"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "util")
                   (:file "option")
                   (:file "subdivide")
                   (:file "pandor")
                   (:file "plet")
                   (:file "pmap")
                   (:file "pmap-open-coded" :if-feature (:not :abcl))
                   (:file "pdotimes")
                   (:file "pquantifier")
                   (:file "preduce")
                   (:file "premove")
                   (:file "pfind")
                   (:file "pcount")
                   (:file "psort")))
                 (:file "package"))))
  :in-order-to ((test-op (test-op :lparallel/test))))

(defmethod perform :after ((o load-op) (c (eql (find-system :lparallel))))
  (declare (ignore o c))
  (pushnew :lparallel *features*))

(defsystem :lparallel/test
  :description "Test suite for lparallel."
  :licence "BSD"
  :author "James M. Lawrence <llmjjmll@gmail.com>"
  :depends-on (:lparallel)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "1am")
                             (:file "package")
                             (:file "base")
                             (:file "thread-util-test")
                             (:file "queue-test")
                             (:file "kernel-test")
                             (:file "cognate-test")
                             (:file "promise-test")
                             (:file "defpun-test")
                             (:file "ptree-test"))))
  :perform (test-op (o c) (symbol-call :lparallel/test '#:execute)))

(defsystem :lparallel/bench
  :description "Benchmarks for lparallel."
  :licence "BSD"
  :author "James M. Lawrence <llmjjmll@gmail.com>"
  :depends-on (:lparallel
               :trivial-garbage
               #+sbcl (:require :sb-sprof))
  :serial t
  :components ((:module "bench"
                :serial t
                :components ((:file "package")
                             (:file "bench")
                             (:file "suite")
                             (:file "profile" :if-feature :sbcl)))))
