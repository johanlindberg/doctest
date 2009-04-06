;;; doctest

(defpackage :doctest
  (:use :cl)
  (:export :test-file
	   :test-function))
(in-package :doctest)

(defun whitespace-p (c)
  "Returns T if <c> is a whitespace character, otherwise NIL."

  (or (equal #\Space c)
      (equal #\Tab c)
      (equal #\Newline c)))

(defun run-doctests (docstring output)
  "Run-doctests is used by test-function and test-file to perform the actual
   work. It returns the number of tests failed and passed and prints to
   <output>."

  (let ((tests-passed 0)
	(tests-failed 0))
    (when docstring
      (do ((c (read-char docstring)
	      (read-char docstring nil 'EOF)))
	  ((eq c 'EOF))
	(when (and (equal #\> c)
		   (equal #\> (read-char docstring))
		   (whitespace-p (peek-char nil docstring)))
	  (let* ((signaled-condition 'nil)
		 (test-form (read docstring))
		 (expected-results (list (read docstring)))
		 (test-results (multiple-value-list
				(handler-case (eval test-form)
				  (condition (co) (progn
						    (setf signaled-condition t)
						    co))))))
	    (if signaled-condition
		(let ((test-condition (first test-results))
		      (expected-condition (first expected-results)))
		  (if (typep test-condition expected-condition)
		      (incf tests-passed)
		      (progn
			(incf tests-failed)
			(format output "~&~A signaled ~A, expected ~A.~%" test-form test-condition expected-condition))))
		(if (equal test-results expected-results)
		    (incf tests-passed)
		    (progn
		      (incf tests-failed)
		      (format output "~&~A returned~{ ~A~}, expected~{ ~A~}.~%" test-form test-results expected-results))))))))
    (values tests-failed tests-passed)))

(defun test-function (function &key (output-stream nil))
  "Test-function extracts and tests code snippets embedded in the documentation
   string of <function>. It returns the number of tests passed and failed and
   prints a description to <output-stream>.

   In order to have a code snippet evaluated as a doctest it must be preceded by
   two '>' characters followed by whitespace. That combination will cause the
   next form to be read and evaluated, and the next form after that to be read
   (but not evaluated).

   Here is the simplest possible example:
   >> 1 ; NOTE! You can use comments to clarify!
   1

   If you excpect more than one value you should wrap it in a multiple-value-
   list to create one form.

   >> (multiple-value-list (values 1 2))
   (1 2)

   NOTE! Newlines and other whitespace (including comments) doesn't particularly
   matter. We could just as well have written >> (multiple-value-list (values 1
   2)) (1 2) instead.

   If you test a function that doesn't have a documentation string, test-
   function will return NIL.
   >> (defun sqr (x)
        (* x x))
   SQR
   >> (test-function #'sqr)
   NIL

   If you need to test that a function signals a condition for certain inputs
   you can use the name of the condition as the expected return value.
   >> (sqr 'x)
   type-error

   If we add a documentation string for sqr with some doctests, we can verify
   that tests can fail as well.
   >> (defun sqr (x)
        \"Returns <x> squared.

          This test will fail:
          >> (sqr 3) 3\"
        (* x x))
   SQR

   Testing foo with test-function should now return 1 and 0.
   >> (multiple-value-list (test-function #'sqr))
   (1 0)"

  (when (documentation function 'function)
    (let ((function-name (third (multiple-value-list (function-lambda-expression function)))))
      (multiple-value-bind (tests-failed tests-passed)
	  (with-input-from-string (docstring (documentation function 'function))
	    (run-doctests docstring output-stream))
	(print-results function-name 'function output-stream tests-failed tests-passed)))))

(defun test-file (filename &key (output-stream nil))
  "test-file extracts and tests code snippets in the contents of <function>. It
   returns the number of tests passed and failed and prints a description to
   <output-stream>."

    (multiple-value-bind (tests-failed tests-passed)
	(with-open-file (docstring filename :direction :input)
	  (run-doctests docstring output-stream))
      (print-results filename 'file output-stream tests-failed tests-passed)))

(defun print-results (test-name test-type output-stream tests-failed tests-passed)
  (format output-stream "~&Results for ~A (~A): ~D of ~D failed.~%" test-name test-type tests-failed (+ tests-failed tests-passed))
  (values tests-failed tests-passed))

