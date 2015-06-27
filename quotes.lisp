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

(defconstant VERSION '(0 0 2))

(load "util.lisp")
(load "backend.lisp")
(load "parser.lisp")

(defvar *default-collection*
	(make-pathname :directory (user-homedir-pathname) :name ".quotes"))

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

(defun user-browse-quote ()
	"The user browses his quote collection"
	(if (y-or-n-p "Load you personal collection? [~A]"
			(namestring *default-collection*))
		(parse-quotefile *default-collection*)
		(progn (format t "~&What collection do you want to load instead?")
			(parse-quotefile (input-string))))
	(setf help-text "
This is the quote browser. Explore your collection by passing commands in the
form \"<all/random> [tag/author]\". 'all' shows you the entire list of quotes
while 'random' picks one out at random. You can narrow down the list of quotes
to choose from by specifying a tag or an author. 'tags' or 'authors' prints the
list of known tags/authors.

Type 'quit' or 'exit' to exit quotes, 'back' to return to the main menu.")
	(format t "~A~%" help-text)
	(input-string command)
	(while (not (or (equalp command "quit")
					(equalp command "exit")
					(equalp command "back")))
		(if (equalp command "help")
			(format t "~A~%" help-text)
			(interpret-command command))
		(input-string command))
	(if (equalp command "back") (main-menu)
		(progn (format t "~&Goodbye!") (quit))))

(defun interpret-command (cmd)
	"Interpret the passed command"
	(unless (listp cmd) (setf cmd (split-string cmd #\space)))
	(let* ((a (first cmd))
			 (b (string-from-list (cdr cmd) #\space))
			  (btype (cond ((equalp b "") 'none)
						 ((member b *author-list* :test #'equalp) 'author)
						 ((member b *tag-list* :test #'equalp) 'tag)
						 (t (format t "Tag/Author not found: ~A" b)
							 (return-from interpret-command)))))
		(cond ((equalp a "all")
				  (case btype
					  ('author (print-quote-list (quotes-by-author b)))
					  ('tag (print-quote-list (quotes-with-tag b)))
					  ('none (print-quote-list *collection*))))
			((equalp a "random")
				(case btype
					('author (print-quote (random-quote :author b)))
					('tag (print-quote (random-quote :tag b)))
					('none (print-quote (random-quote)))))
			((equalp a "tags")
				(format t "~&~A" (string-from-list *tag-list*)))
			((equalp a "authors")
				(format t "~&~A" (string-from-list *author-list*)))
			(t (format t "Bad command: ~A" (string-from-list cmd #\space))))))

(defun print-quote (q)
	"Pretty-print a quote"
	(format t "~&~A~%(~A)" (quotation-text q) (quotation-author q)))

(defun print-quote-list (ql)
	"Pretty print this list of quotes"
	(cond ((null ql) NIL)
		((= (length ql) 1) (print-quote (car ql)))
		(t (print-quote (car ql))
			(format t "~&===")
			(print-quote-list (cdr ql)))))

(defun user-add-quote ()
	"The user adds a quote"
	;; TODO save to file!
	(let ((q (make-quotation)))
		(format t "~&Please type in the quote:")
		(setf (quotation-text q) (input-string))
		(format t "~&Who is the author of this quote?")
		(setf (quotation-author q) (input-string))
		(format t "~&Please assign tags to this quote. ")
		(format t "Tags must be comma-separated.")
		(setf (quotation-tags q)
			(extract-tags (concatenate 'string "tags:" (input-string))))
		(format t "~&Quote added.")
		(print q)
		(add-quote q)
		(if (y-or-n-p "Add another quote?")
			(user-add-quote) (main-menu))))

(defun main-menu ()
	(print-version)
	(format t "~&~%Welcome to quotes! What do you want to do?")
	(case (choose-number-option '("Browse your quotes" "Add a quote" "Quit"))
		(0 (user-browse-quote))
		(1 (user-add-quote))
		(2 (format t "~&Goodbye!") (quit))))

;; Only show the interactive menu if no commandline parameters are given
(if *args*
	(parse-commandline-args)
	(main-menu))
