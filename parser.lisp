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

(defun parse-quotefile (quote-file)
	"Parse the given file, transforming it into a list of quotes"
	(do* ((quote-list nil) (lines (remove-comments (load-text-file quote-file)))
			 (current-quote NIL) (line-nr 0 (1+ line-nr))
			 (line (nth line-nr lines) (nth line-nr lines)))
		((null line) quote-list)
		;; concatenate multi-line entries (indicated by indentation)
		;; FIXME doesn't remove the indented lines
		(do* ((next-line-nr (1+ line-nr) (1+ next-line-nr))
				 (next-line (nth next-line-nr lines) (nth next-line-nr lines)))
			((or (null next-line) (not (or (eql (aref next-line 0) #\space)
										  (eql (aref next-line 0) #\tab)))) NIL)
			(setf line (concatenate 'string line (trim-whitespace next-line))))
		(break)
		;; start a new quote
		(if (equalp "[quote]" (trim-whitespace line))
			(progn
				(when current-quote
					(setf quote-list (append quote-list (list current-quote))))
				(setf current-quote (make-quotation)))
			;; interpret the first word of the line
			;; FIXME doesn't seem to do anything :-(
			(case (first (cut-string line (position #\: line)))
				("author:" (setf (quotation-author current-quote)
							   (trim-whitespace
								   (second (cut-string line 7)))))
				("text:" (setf (quotation-text current-quote)
							 (trim-whitespace
								 (second (cut-string line 5)))))
				("tags:" (setf (quotation-tags current-quote)
							 (extract-tags line)))))))

(defun remove-comments (lines)
	"Remove comments and empty lines"
	(cond ((or (null (car lines))(null lines)) NIL)
		((or (equalp (car lines) "") (eql (aref (car lines) 0) #\;))
			(remove-comments (cdr lines)))
		(t (cons (car lines) (remove-comments (cdr lines))))))

(defun extract-tags (line)
	"Extract a list of tags from a collection file line"
	(let* ((tag-string (second (cut-string line 5))) (tags nil))
		(dolist (tag (split-string tag-string #\,) tags)
			(setf tags (append tags (list (trim-whitespace tag)))))))
