;;;
;;; quotes is a small Unix utility for creating, organizing 
;;; and accessing your own collection of quotes and citations.
;;;
;;; This module houses the backend, where the actual handling
;;; of quotes takes place.
;;;
;;; Licensed under the terms of the MIT license.
;;; author: Daniel Vedder
;;; date: 25/06/2015
;;;

(defstruct quotation
	(author "")
	(text "")
	(tags NIL)
	(notes ""))

(defvar *collection* NIL)

(defun add-quote (quotation)
	(setf *collection* (append *collection* (list quotation))))

(defun quotes-with-tag (tag)
	"Return a list of all quotes with this tag."
	(let ((quote-list NIL))
		(dolist (q *collection* quote-list)
			(when (member tag (quotation-tags q) :test #'equalp)
				(setf quote-list (append quote-list (list q)))))))
