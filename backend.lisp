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
	(tags NIL))

(defvar *collection* NIL)
(defvar *author-list* NIL)
(defvar *tag-list* NIL)

;; initialize the random state
(setf *random-state* (make-random-state t))

(defun add-quote (quotation)
	"Add a quote to the collection"
	;; index the tags of this quotation
	(dolist (tag (quotation-tags quotation))
		(unless (member tag *tag-list* :test #'equalp)
			(setf *tag-list* (cons tag *tag-list*))))
	;; index the author
	(unless (member (quotation-author quotation) *author-list* :test #'equalp)
		(setf *author-list* (cons (quotation-author quotation) *author-list*)))
	;; add the quote to the collection
	(setf *collection* (append *collection* (list quotation))))

(defun quotes-with-tag (tag)
	"Return a list of all quotes with this tag."
	(let ((quote-list NIL))
		(dolist (q *collection* quote-list)
			(when (member tag (quotation-tags q) :test #'equalp)
				(setf quote-list (append quote-list (list q)))))))

(defun quotes-by-author (author)
	"Return a list of all quotes by this author"
	(let ((quote-list NIL))
		(dolist (q *collection* quote-list)
			(when (equalp author (quotation-author q))
				(setf quote-list (append quote-list (list q)))))))

(defun random-quote (&key (tag nil) (author nil))
	"Return a random quote (by this tag/author, if defined)"
	;; TODO Simplify this with macrolet?
	(cond ((not (or tag author))
			  (nth (random (length *collection*)) *collection*))
		((and tag (not author))
			(let ((quote-list (quotes-with-tag tag)))
				(nth (random (length quote-list)) quote-list)))
		((and author (not tag))
			(let ((quote-list (quotes-by-author author)))
				(nth (random (length quote-list)) quote-list)))
		((and author tag)
			(let ((quote-list (intersection (quotes-by-author author)
								  (quotes-with-tag tag))))
				(nth (random (length quote-list)) quote-list)))))
