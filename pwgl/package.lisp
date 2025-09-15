(defpackage :AB93 (:use :cl))
(in-package :AB93)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (import '(ccl::PWGLdef) :AB93))