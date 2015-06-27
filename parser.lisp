;;;
;;; Quotes is a small Unix utility for creating, organizing 
;;; and accessing your own collection of quotes and citations.
;;;
;;; This module loads and parses a quote collection.
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 25/06/2015
;;;

(defun parse-quotefile (quote-file &optional (build-*collection* t))
	"Parse the given file, transforming it into a list of quotes"
	;; This function returns a list of quotes. If build-*collection* is
	;; true, each quote is also automatically added to the *collection*.
	;; (As it represents a side-effect, this behaviour can be switched off.)
	(when build-*collection* (setf *collection* NIL))
	(do* ((quote-list nil) (current-quote NIL)
			 (lines (concatenate-multilines
						(remove-comments (load-text-file quote-file))))
			 (line-nr 0 (1+ line-nr))
			 (line (nth line-nr lines) (nth line-nr lines)))
		((null line)
			(when current-quote
				(setf quote-list (append quote-list (list current-quote)))
				(when build-*collection* (add-quote current-quote)))
			quote-list)
		;; start a new quote
		(if (equalp "[quote]" (trim-whitespace line))
			(progn
				(when current-quote
					(setf quote-list (append quote-list (list current-quote)))
					(when build-*collection* (add-quote current-quote)))
				(setf current-quote (make-quotation)))
			;; interpret the first word of the line (the keyword)
			(let ((keyword (first (cut-string line (1+ (position #\: line))))))
				(cond ((equalp keyword "author:")
						  (setf (quotation-author current-quote)
							  (trim-whitespace (second (cut-string line 7)))))
					((equalp keyword "text:")
						(setf (quotation-text current-quote)
							(trim-whitespace (second (cut-string line 5)))))
					((equalp keyword "tags:")
						(setf (quotation-tags current-quote)
							(extract-tags line)))
					(t (error "Unrecognized construct: ~A" line)))))))

(defun remove-comments (lines)
	"Remove comments and empty lines"
	(cond ((or (null (car lines))(null lines)) NIL)
		((or (equalp (car lines) "") (eql (aref (car lines) 0) #\;))
			(remove-comments (cdr lines)))
		(t (cons (car lines) (remove-comments (cdr lines))))))

(defun concatenate-multilines (lines)
	"Concatenate multilines in this list of strings"
	;; Multilines are denoted by indentation in subsequent lines
	(let ((next (cadr lines)))
		(cond ((null lines) NIL)
			((and next (or (eql (aref next 0) #\space)
						   (eql (aref next 0) #\tab)))
				(concatenate-multilines
					(cons (concatenate 'string
							  (trim-whitespace (car lines) 'right)  " "
							  (trim-whitespace next))
						(cddr lines))))
			(t (cons (car lines) (concatenate-multilines (cdr lines)))))))

(defun extract-tags (line)
	"Extract a list of tags from a collection file line"
	(let* ((tag-string (second (cut-string line 5))) (tags nil))
		(dolist (tag (split-string tag-string #\,) tags)
			(setf tags (append tags (list (trim-whitespace tag)))))))

(defun save-quote (q quotefile)
	"Save a quotation to file"
	(let ((quote-string NIL))
		(setf quote-string
			(append quote-string (list (format nil "~&~%[Quote]"))))
		(setf quote-string
			(append quote-string
				(list (format NIL "author: ~A" (quotation-author q)))))
		(setf quote-string
			(append quote-string
				(list (format NIL "text: ~A" (quotation-text q)))))
		(setf quote-string
			(append quote-string
				(list (format NIL "tags: ~A"
						  (string-from-list (quotation-tags q) ", ")))))
		(write-to-file quote-string quotefile t)))


