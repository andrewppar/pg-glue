;;; pg-glue-utils.el --- Glue code for postgres, pg.el, and some other tools -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andrew Parisi

;;; Commentary:
;;
;; This is a bunch of utilities for dealing with plists in a functional
;; way.
;;
;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defun pg-glue-utils/get (plist key)
  "Get the value of PLIST at KEY using test EQUAL."
  (plist-get plist key #'equal))

(defun pg-glue-utils/get-in (plist keys)
  "Get the value of PLIST at KEYS."
  (seq-reduce
   (lambda (acc key)
     (pg-glue-utils/get acc key))
   keys
   plist))

(defun pg-glue-utils/assoc (plist key value)
  "Like `plist-put` except that VALUE is added to PLIST at KEY with test EQUAL."
  (plist-put plist key value #'equal))

(defun pg-glue-utils/assoc-in (plist keys value)
  "Add VALUE to PLIST at KEYS."
  (cond ((= (length keys) 1)
	 (pg-glue-utils/assoc plist (car keys) value))
	(keys
	 (let* ((key (car keys))
		(next-plist (pg-glue-utils/get plist key))
		(recursive (pg-glue-utils/assoc-in
			    next-plist (cdr keys) value)))
	   (pg-glue-utils/assoc plist key recursive)))
	(t plist)))

(defun pg-glue-utils/update (plist key update-fn &rest args)
  "Update the existing value of PLIST at KEY by calling UPDATE-FN and ARGS on it."
  (let ((existing (plist-get plist key #'equal)))
    (plist-put plist key (apply update-fn existing args) #'equal)))

(defun pg-glue-utils/update-in (plist keys update-fn &rest args)
  "Upadte the value at KEYS in PLIST by calling UPDATE-FN on it with ARGS."
  (let ((new-value (apply update-fn (pg-glue-utils/get-in plist keys) args)))
    (pg-glue-utils/assoc-in plist keys new-value)))

(defun pg-glue-utils/do-plist
    (plist collect-fn stop-fn &optional initial-value)
  "Loop over PLIST collecting results with COLLECT-FN and stopping at STOP-FN.
COLLECT-FN expects a result key and value, and should return the updated values
of result.
STOP-FN takes a result and determines whether it should stop iteration.
Optionally pass INITIAL-VALUE to start with COLLECT-FN."
  (let ((result initial-value))
    (cl-do ((todo (cddr plist) (cddr todo))
	    (key (car plist) (car todo))
	    (val (cadr plist) (cadr todo)))
	   ((or (and (not key) (not val)) (funcall stop-fn result)) result)
      (setq result (funcall collect-fn result key val)))))

(defun pg-glue-utils/keys (plist)
  "Get all the keys of PLIST."
  (pg-glue-utils/do-plist
   plist
   (lambda (result key _val)
     (cons key result))
   (lambda (_result) nil)))

(defun pg-glue-utils/vals (plist)
  "Get all the vals of PLIST."
  (pg-glue-utils/do-plist
   plist
   (lambda (result _key val)
     (cons val result))
   (lambda (_result) nil)))

(provide 'pg-glue-utils)
;;; pg-glue-utils.el ends here
