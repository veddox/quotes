;;;
;;; quotes is a small Unix utility for creating, organizing 
;;; and accessing your own collection of quotes and citations.
;;;
;;; This is the main (frontend) module, responsible for commandline
;;; parameter parsing and the text-based UI.
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 25/06/2015
;;;

(defconstant VERSION '(0 0 1))

(load "util.lisp")
(load "backend.lisp")
(load "parser.lisp")

(defun print-version ()
	(format t "~&quotes ~A.~A.~A"
		(first VERSION)
		(second VERSION)
		(third VERSION))
	(format t "~&Copyright (c) 2015 Daniel Vedder")
	(format t "~&Licensed under the terms of the MIT license.~%"))

(defun print-help ()
	(print-version)
	(setf help-text "
Commandline options:
-v --version          Show the version number and exit
-h --help             Show this help text and exit
-i --interactive      Launch interactive mode (default)")
	(format t "~A" help-text))

(defun cmd-parameter (name &optional truth-value)
	"Return the value of the parameter 'name'. Or T for present if truth-value."
	(let ((argument (member name *args* :test #'equalp)))
		(if argument
			(if truth-value T (second argument))
			NIL)))

(defun parse-commandline-args ()
	(cond ((or (cmd-parameter "--version" T) (cmd-parameter "-v" T))
			  (print-version) (quit))
		((or (cmd-parameter "--help" T) (cmd-parameter "-h" T))
			(print-help) (quit))
		((or (cmd-parameter "--interactive" T) (cmd-parameter "-i" T))
			(main-menu))
		;; TODO
		))

(defun main-menu ()
	;; TODO
	)

;; Only show the interactive menu if no commandline parameters are given
(if *args*
	(parse-commandline-args)
	(main-menu))


(defun test ()
	(setf *collection* NIL)
	(add-quote (make-quotation :author "Jesus" :text "It is finished" :tags '("Bible")))
	(add-quote (make-quotation :author "John" :text "Jesus wept" :tags '("Bible")))
	(add-quote (make-quotation :author "Mark Twain" :text "The world doesn't owe you anything. It was here first." :tags '("life" "determination"))))

(test)
