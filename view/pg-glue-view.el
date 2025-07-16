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

(defun pg-glue-view--parse-cell (cell)
  "Parse CELL into the value it will be shown as."
  (string-trim
   (cond ((numberp cell)
	  (format "%s" cell))
	 ((stringp cell)
	  cell)
	 ((and cell (listp cell))
	  (format-time-string "%Y-%m-%dT%H:%M:%S" (encode-time cell)))
	 ((not cell)
	  "")
	 (t cell))))

(defun pg-glue-view--key-maxes (plists)
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
		(length (pg-glue-view--parse-cell y))))
	 value))
      (lambda (_x) nil)
      maxes))
   plists
   '()))

(defun pg-glue-view/format-cell (plist padding-spec key)
  "Create a padded cell for KEY from PLIST.
The max values for key from PADDING-SPEC is used to calcuate padding."
  (let* ((cell (pg-glue-view--parse-cell (or (pg-glue-utils/get plist key) "")))
	 (pad-amount (thread-last
		       cell
		       length
		       (- (or (pg-glue-utils/get padding-spec key) 0))
		       (+ 1))))
    (format "%s%s" cell (make-string pad-amount ?\ ))))

(defconst pg-glue-view/red "#f5e0dc")
(defconst pg-glue-view/light-blue "#89dceb")
(defconst pg-glue-view/blue "#89b4fa")
(defconst pg-glue-view/green "#a6e3a1")
(defconst pg-glue-view/grey "#a6adc8")
(defconst pg-glue-view/orange "#fab387")
(defconst pg-glue-view/yellow "#f9e2af")

(defun pg-glue-view--colorize (text color)
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

(defun pg-glue-view/schema (schema tables)
  "Draw a table view for TABLES in SCHEMA."
  (let ((header (format "Tables: %s" schema)))
    (string-join
     (append
      (list header
	    (pg-glue-view--colorize
	     (make-string (length header) ?\=) :light-blue))
      tables)
     "\n")))

(defun pg-glue-view/color-column-type (column-type)
  "Add color to COLUMN-TYPE."
  (pg-glue-view--colorize
   column-type (cond ((equal (string-trim column-type) "uuid") :yellow)
		     ((equal (string-trim column-type) "jsonb") :blue)
		     ((equal (string-trim column-type) "boolean") :orange)
		     ((equal (string-trim column-type)
			     "timestamp with time zone") :grey)
		     ((equal (string-trim column-type)
			     "timestamp without time zone") :grey)
		     (t 'default))))

(defun pg-glue-view/primary-key (primary-key)
  "Draw a view of PRIMARY-KEY."
  (let ((name (pg-glue-utils/get primary-key "name"))
	(columns (pg-glue-utils/get primary-key "columns")))
    (string-join
     (list (pg-glue-view--colorize "primary key" :light-blue)
	   (format "%s on %s" (pg-glue-view--colorize name :green) columns))
     "\n")))


(defun pg-glue-view--keys (foreign-keys direction)
  "Draw FOREIGN-KEYS in DIRECTION."
  (let* ((divider (cl-case direction
		    (:foreign (pg-glue-view--colorize "<---" :yellow))
		    (:domestic (pg-glue-view--colorize "--->" :green))))
	 (key (substring (format "%s" direction) 1))
	 (opposite
	  (cl-case direction (:foreign "domestic") (:domestic "foreign")))
	 (table-key (format "%s-table" opposite))
	 (column-key (format "%s-columns" opposite))
	 (specs (mapcar
		 (lambda (foreign-key)
		   (list
		    :name (pg-glue-view--colorize
			   (pg-glue-utils/get foreign-key "name")
			   :red)
		    :columns (format "%s" (pg-glue-utils/get foreign-key "columns"))
		    :divider divider
		    :other-table (pg-glue-utils/get foreign-key table-key)
		    :other-columns (format "%s" (pg-glue-utils/get foreign-key column-key))))
		 (pg-glue-utils/get foreign-keys (format "%s-key" key))))
	 (maxes (pg-glue-view--key-maxes specs)))
    (string-join
     (mapcar
      (lambda (foreign-key)
	(seq-reduce
	 (lambda (result key)
	   (format "%s%s"
		   result (pg-glue-view/format-cell foreign-key maxes key)))
	 (list :name :columns :divider :other-table :other-columns)
	 ""))
      specs)
     "\n")))

(defun pg-glue-view/foreign-keys (foreign-keys)
  "Draw a table for the foreign-keys of FOREIGN-KEYS."
  (pg-glue-view--keys foreign-keys :foreign))

(defun pg-glue-view/domestic-keys (foreign-keys)
  "Draw a table for the domestic-keys of FOREIGN-KEYS."
  (pg-glue-view--keys foreign-keys :domestic))

(defun pg-glue-view/columns (schema table columns)
  "Draw the COLUMNS for TABLE in SCHEMA."
  (let* ((header (format "Table: %s.%s" schema table))
	 (maxes (pg-glue-view--key-maxes columns))
	 (divider (pg-glue-view--colorize
		   (make-string (+ (apply #'+ (pg-glue-utils/vals maxes)) 2) ?\=)
		   :light-blue)))
    (insert
     (string-join
      (append (list header divider)
	      (mapcar
	       (lambda (column)
		 (let* ((name-cell (pg-glue-view/format-cell
				    column maxes "name"))
			(type-cell (pg-glue-view/color-column-type
				    (pg-glue-view/format-cell
				     column maxes "type")))
			(default-cell (pg-glue-view/format-cell
				       column maxes "default")))
		   (format "|%s|%s|%s" name-cell type-cell default-cell)))
	       columns)
	      (list divider))
      "\n"))))

(defun pg-glue-view/table (schema table columns primary-key foreign-keys)
  "Describe TABLE in SCHEMA with COLUMNS and FOREIGN-KEYS."
  (string-join
   (list
    (pg-glue-view/columns schema table columns)
    ""
    (pg-glue-view/primary-key primary-key)
    ""
    (pg-glue-view--colorize "foreign keys" :light-blue)
    (pg-glue-view/domestic-keys foreign-keys)
    (pg-glue-view/foreign-keys foreign-keys))
   "\n"))

(defun pg-glue-view/query-result (query-results)
  "Format QUERY-RESULTS to be displayed in a standalone buffer.
NOTE: QUERY-RESULTS has to be returned as ':table from pg-query-query/query."
  (let* ((columns (reverse (pg-glue-utils/keys (car query-results))))
	 (header-row (mapcan (lambda (column) (list column column)) columns))
	 (maxes (pg-glue-view--key-maxes (cons header-row query-results)))
	 (header (format "|%s|"
			 (string-join
			  (mapcar
			   (lambda (column)
			     (pg-glue-view/format-cell header-row maxes column))
			   columns)
			  "|")))
	 (divider (string-replace "|" "+" (replace-regexp-in-string "[^|]" "-" header)))
	 (formatted-rows (mapcar
			  (lambda (row)
			    (format "|%s|"
				    (string-join
				     (mapcar
				      (lambda (column)
					(pg-glue-view/format-cell row maxes column))
				      columns)
				     "|")))
			  query-results))
	 (total (format "rows: %s" (length query-results)))
	 (body (reverse (cons total (cons divider (reverse formatted-rows))))))
    (string-join
     (cons divider (cons header (cons divider body)))
     "\n")))

(provide 'pg-glue-view)
;;; pg-glue-view.el ends here
