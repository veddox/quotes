;;;
;;; quotes is a small Unix utility for creating, organizing 
;;; and accessing your own collection of quotes and citations.
;;;
;;; This is a module for generic utility functions and macros.
;;; (Mainly copied from the Atlantis project.)
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 25/06/2015
;;;

(defmacro input (var &optional (prompt ">>>"))
	"Take input from terminal and store it in var"
	`(progn
		 (format t "~&~A " ,prompt)
		 (setf ,var (read))))

(defmacro input-string (&optional (var (gensym)))
	"Read a string input line"
	`(progn
		 (format t "~&>>> ")
		 (setf ,var (read-line))))

(defmacro while (condition &body body)
	"An implementation of a while loop as found in other languages"
	`(do ()
		 ((not ,condition) NIL)
		 ,@body))

(defun split-string (str separator)
	"Split the string up into a list of strings along the separator character"
	(cond ((equalp str (to-string separator)) NIL)
		((zerop (count-instances separator str)) (list str))
		(T (let ((split-elt (cut-string str (position separator str))))
			   (cons (first split-elt)
				   (split-string (second (cut-string (second split-elt) 1))
					   separator))))))

(defun cut-string (s i)
	"Cut string s in two at index i and return the two substrings in a list"
	(do* ((c 0 (1+ c)) (letter (aref s c) (aref s c))
			(letter-list-1 NIL) (letter-list-2 NIL))
		((= c (1- (length s)))
			(list (list-to-string (append letter-list-1))
				(list-to-string (append letter-list-2 (list letter)))))
		(if (< c i) (setf letter-list-1 (append letter-list-1 (list letter)))
			(setf letter-list-2 (append letter-list-2 (list letter))))))

(defun to-string (x)
	"Whatever x is, convert it into a string"
	(cond ((stringp x) x)
		((or (symbolp x) (characterp x)) (string x))
		(t (format NIL "~S" x))))

(defun load-text-file (file-name)
	"Load a text file into a list of strings (representing the lines)"
	(with-open-file (f file-name)
		(do* ((line (read-line f nil nil)
				  (read-line f nil nil))
				 (file-lines (list line) (append file-lines (list line))))
			((null line) file-lines))))

(defun choose-number-option (option-list)
	"The user chooses one out of a list of options, the index is returned"
	(dotimes (i (length option-list))
		(format t "~&~S) ~A" (1+ i) (nth i option-list)))
	(input choice)
	(while (or (not (numberp choice)) (< choice 1)
			   (> choice (length option-list)))
		(format t "~&Invalid choice! Please choose again:")
		(input choice))
	(1- choice))
