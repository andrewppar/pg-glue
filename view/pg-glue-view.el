;;; pg-glue-view.el --- Glue code for postgres, pg.el, and some other tools -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andrew Parisi

;;; Commentary:
;;
;; Make viewing things a little easier
;;
;;; Code:
(require 'cl-lib)
(require 'pg-glue-utils)
(require 'subr-x)


(defun pg-glue-view/key-maxes (plists)
  "The maximum value for each column in PLISTS."
  (seq-reduce
   (lambda (maxes plist)
     (pg-glue-utils/do-plist
      plist
      (lambda (result key value)
	(pg-glue-utils/update
	 result key
	 (lambda (x y)
	   (max (or x 0)
		(if (numberp y) y (length y))))
	 value))
      (lambda (_x) nil)
      maxes))
   plists
   '()))

(defun pg-glue-view/format-cell (plist padding-spec key)
  "Create a padded cell for KEY from PLIST.
The max values for key from PADDING-SPEC is used to calcuate padding."
  (let* ((cell-string (or (pg-glue-utils/get plist key) ""))
	 (pad-amount (thread-last
		       cell-string
		       length
		       (- (or (pg-glue-utils/get padding-spec key) 0))
		       (+ 1))))
    (format "%s%s" cell-string (make-string pad-amount ?\ ))))

(defconst pg-glue-view/red "#f5e0dc")
(defconst pg-glue-view/light-blue "#89dceb")
(defconst pg-glue-view/blue "#89b4fa")
(defconst pg-glue-view/green "#a6e3a1")
(defconst pg-glue-view/grey "#a6adc8")
(defconst pg-glue-view/orange "#fab387")
(defconst pg-glue-view/yellow "#f9e2af")

(defun pg-glue-view/colorize (text color)
  "Draw TEXT in COLOR."
  (let ((hex-color (cl-case color
		     (:red pg-glue-view/red)
		     (:light-blue pg-glue-view/light-blue)
		     (:blue pg-glue-view/blue)
		     (:green pg-glue-view/green)
		     (:grey pg-glue-view/grey)
		     (:orange pg-glue-view/orange)
		     (:yellow pg-glue-view/yellow))))
    (propertize text 'face `(:foreground ,hex-color))))

(defun pg-glue-view/color-column-type (column-type)
  "Add color to COLUMN-TYPE."
  (pg-glue-view/colorize
   column-type (cond ((equal (string-trim column-type) "uuid") :yellow)
		     ((equal (string-trim column-type) "jsonb") :blue)
		     ((equal (string-trim column-type) "boolean") :orange)
		     ((equal (string-trim column-type)
			     "timestamp with time zone") :grey)
		     ((equal (string-trim column-type)
			     "timestamp without time zone") :grey)
		     (t 'default))))



(provide 'pg-glue-view)
;;; pg-glue-view.el ends here
