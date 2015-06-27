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

(defun count-instances (search-term search-sequence &key (test #'eql))
	"Count the number of instances of search-term in search-sequence"
	(let ((count 0))
		(dotimes (i (length search-sequence) count)
			(when (funcall test search-term (elt search-sequence i))
				(incf count)))))

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

(defun list-to-string (char-list)
	"Convert a character list to a string"
	(let ((s (make-string (length char-list) :initial-element #\SPACE)))
		(dotimes (i (length char-list) s)
			(setf (aref s i) (nth i char-list)))))

(defun string-from-list (lst &optional (separator " - "))
	"Put all elements of lst into a single string, separated by the separator"
	(cond ((null lst) "")
		((= (length lst) 1) (to-string (car lst)))
		(T (concatenate 'string (to-string (first lst)) (to-string separator)
			(string-from-list (cdr lst) separator)))))

(defun trim-whitespace (s &optional (side 'both))
	"Trim off spaces and tabs before and after string s"
	(let ((whitespace '(#\space #\tab)))
		(case side
			('left (string-left-trim whitespace s))
			('right (string-right-trim whitespace s))
			(t (string-trim whitespace s)))))

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

(defun write-to-file (text filename &optional (append NIL))
	"Write text (a string or list of strings) to the specified file"
	(let ((text-list (if (listp text) text (list text)))
			 (f (if append
					(open filename :direction :output :if-exists :append)
					(open filename :direction :output))))
		(dolist (line text-list)
			(format f "~&~A~&" line))
		(close f)))

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

