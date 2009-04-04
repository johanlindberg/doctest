;;; doctest

(defun test-function (function &key (output nil))
  "test-function extracts and tests code snippets embedded in the documentation
   string of <function>. It returns the number of tests passed and failed and
   prints a description to <output>.

   In order to have a code snippet evaluated as a doctest it must be preceded by
   3 '>' characters. That will cause the next form to be read and evaluated, and
   the next N forms after that to be read (but not evaluated). Where N is the
   number of values the form returns.

   Here is the simplest possible example:
   >>> 1 ; You can use comments to clarify
   1

   And here is one with more than one return value.
   >>> (values 1 2)
   1
   2
   
   Note that newlines and other whitespace (including comments) doesn't
   particularly matter. We could have added the test >>> 1 1 more or less
   anywhere within the docstring.

   Another thing to keep in mind is that results are only compared against as
   many forms as it returns, so this test:
   >>> (values 1 2)
   1
   2
   3

   will pass because 3 is considered text and ignored.

   Here is a slightly more complicated example:
   >>> (defun foo (x)
         (* x x))
   FOO
   >>> (foo 1) 1
   >>> (foo 2) 4

   Since foo doesn't have any doctests specified, test-function will return 0 0.
   >>> (test-function #'foo) 0 0

   Let's specify a documentation string for foo and add some doctests to it.
   >>> (defun foo (x)
         \"Foo returns x squared.

           >>> (foo 1) 1
           >>> (foo 2) 4

           This test will fail:
           >>> (foo 3) 3\"
         (* x x))
   FOO
   >>> (test-function #'foo) 1 2"
  (let ((count 0)
	(tests-passed 0)
	(tests-failed 0))
    (when (documentation function 'function)
      (with-input-from-string (docstring (documentation function 'function))
	(do ((c (read-char docstring)
		(read-char docstring nil 'EOF)))
	    ((eq c 'EOF))

	  (if (equal #\> c)
	      (progn
		(incf count)
		(when (eq count 3)
		  (let* ((test-form (read docstring))
			 (test-result (multiple-value-list (eval test-form)))
			 (expected-result (mapcar #'(lambda (x)
						      (declare (ignore x))
						      (read docstring))
						  test-result)))
		    (if (equal test-result expected-result)
			(incf tests-passed)
			(progn
			  (incf tests-failed)
			  (format output "~&~A returned~{ ~A~}, expected~{ ~A~}.~%" test-form test-result expected-result))))
		  (setf count 0)))
	      (setf count 0)))))

    (format output "~&Results for #'~A: ~D of ~D failed.~%" (function-name function) tests-failed (+ tests-failed tests-passed))
    (values tests-failed tests-passed)))