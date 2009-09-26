(defpackage :doctest-system
  (:use :common-lisp :asdf))
(in-package :doctest-system)

(defsystem :doctest
  :description "Doctests for Common Lisp"
  :version "0.1"
  :author "Johan Lindberg, Pulp Software <johan@pulp.se>"
  :licence "GPL"
  :components ((:file "doctest")))
