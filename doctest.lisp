;;; Doctests for Common Lisp.
;;; Copyright (C) 2009 Johan Lindberg, Pulp Software

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :doctest
  (:use :common-lisp)
  (:export :test-file
	   :test-function))
(in-package :doctest)

(defun whitespace-p (c)
  "Returns T if <c> is a whitespace character, otherwise NIL."

  (or (equal #\Space c)
      (equal #\Tab c)
      (equal #\Newline c)))

(defun remove-ws (string)
  "Return <string> (as a string) with *all* whitespace characters removed."

  (if (stringp string)
      (remove-if #'whitespace-p (copy-seq string))
      (remove-if #'whitespace-p (copy-seq (string string)))))
    
(defun string-equal-ignore-ws (string1 string2)
  (string-equal (remove-ws string1) (remove-ws string2)))

(defun run-doctests (docstring output)
  "Run-doctests is used by test-function and test-file to perform the actual
   work. It returns the number of tests failed and passed and prints to
   <output>."

  ;; TODO: Needs refactoring! Break this up into two methods (at least), one for
  ;; setting up/finding tests and one for executing them.
  (let ((tests-failed 0)
	(tests-passed 0))
    (when docstring
      (do ((c (read-char docstring)
	      (read-char docstring nil 'EOF)))
	  ((eq c 'EOF))
	(when (and (equal #\> c)
		   (equal #\> (read-char docstring))
		   (whitespace-p (peek-char nil docstring)))
	  (let ((test-form-signaled-condition 'nil)
		(test-form (read docstring)))
	    (let ((expected-result (list (read docstring))))
	      (let ((actual-output (make-array '(0)
					       :element-type 'base-char
					       :fill-pointer 0
					       :adjustable t))
		    (expected-output '()))
		(when (and (symbolp (car expected-result))
			   (equal (string (car expected-result)) "->"))
		  (setf expected-output (read docstring))
		  (setf expected-result (list (read docstring))))

		(let ((actual-result (multiple-value-list
				      (handler-case (with-output-to-string (*standard-output* actual-output)
						      (eval test-form))
					(condition (co) (progn
							  (setf test-form-signaled-condition t)
							  co))))))
		  (unless expected-output
		    (setf actual-output '()))

		  (let ((expected-output-matches-actual-output
			 (string-equal-ignore-ws actual-output expected-output)))
		    (if test-form-signaled-condition
			(if (typep (car actual-result) (car expected-result))
			    (incf tests-passed)
			    (progn
			      (incf tests-failed)
			      (format output "~&~A signaled ~A, expected ~A.~%" test-form (car actual-result) (car expected-result))))
			
			(if (and (equal actual-result expected-result)
				 expected-output-matches-actual-output)
			    (incf tests-passed)
			    (progn
			      (incf tests-failed)
			      (if expected-output-matches-actual-output
				  (format output "~&~A returned~{ ~A~}, expected~{ ~A~}.~%" test-form actual-result expected-result)
				  (format output "~&~A printed \"~A\", expected \"~A\".~%" test-form actual-output expected-output)))))))))))))
    (values tests-failed tests-passed)))

(defun test-function (function &key (output nil))
  "Test-function extracts and tests code snippets embedded in the documentation
   string of <function>. It returns the number of tests failed and passed and
   prints a description to <output>.

   In order to have a code snippet evaluated as a doctest it must be preceded by
   two '>' characters followed by whitespace. That combination will cause the
   next form to be read and evaluated, and the next or the two next forms after
   that to be read (but not evaluated).

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
   TYPE-ERROR

   If we add a documentation string for sqr with a doctest, we can verify that
   tests can fail as well.
   >> (defun sqr (x)
        \"Returns <x> squared.

          This test will fail:
          >> (sqr 3) 3\"
        (* x x))
   SQR

   Testing sqr with test-function should now return 1 failed and 0 passed.
   >> (multiple-value-list (test-function #'sqr))
   (1 0)

   If you need to test the output of a function you can add an expected-
   output form *between* the function call and the return value. It must
   consist of two atoms so you should either use a string or wrap the expected
   output in '|' characters.
   >> (defun sqr (x)
        \"Prints <x> and <x>*<x> to standard output and returns NIL.

          This test will pass,
          >> (sqr 2)
          -> |2 * 2 = 4|
          NIL

          as will this, because it ignores the output.
          >> (sqr 2)
          NIL

          This test will fail because expected output doesn't match the
          actual output.
          >> (sqr 2)
          -> |Blah blah blah|
          NIL\"
        (format t \"~A * ~A = ~A\" x x (* x x)))
   SQR

   Testing sqr with test-function should now return 1 failed and 2 passed. It
   should also inform us that:

   (SQR 2) printed \"2 * 2 = 4\", expected \"Blah blah blah\".
   Results for SQR (FUNCTION): 1 of 3 failed.

   NOTE! Whitespace is ignored when output is compared.

   >> (multiple-value-list (test-function #'sqr :output T))
   -> |(SQR 2) printed \"2 * 2 = 4\", expected \"Blah blah blah\". Results for SQR (FUNCTION): 1 of 3 failed.|
   (1 2)"

  (when (documentation function 'function)
    (let ((function-name (third (multiple-value-list (function-lambda-expression function)))))
      (multiple-value-bind (tests-failed tests-passed)
	  (with-input-from-string (docstring (documentation function 'function))
	    (run-doctests docstring output))
	(print-results function-name 'function output tests-failed tests-passed)))))

(defun test-file (filename &key (output nil))
  "Test-file extracts and tests code snippets in the contents of <filename>. It
   returns the number of tests failed and passed and prints a description to
   <output>.

   See also the documentation string for test-function."

    (multiple-value-bind (tests-failed tests-passed)
	(with-open-file (docstring filename :direction :input)
	  (run-doctests docstring output))
      (print-results filename 'file output tests-failed tests-passed)))

(defun print-results (test-name test-type output tests-failed tests-passed)
  (format output "~&Results for ~A (~A): ~D of ~D failed.~%" test-name test-type tests-failed (+ tests-failed tests-passed))
  (values tests-failed tests-passed))
