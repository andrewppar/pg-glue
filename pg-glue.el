;; pg-glue.el --- Glue code for postgres, pg.el, and some other tools -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Createed: March 12, 2025
;; Version 0.1
;; Package-Requires: ((pg "0.49") (eros "0.1.0"))
;; Keywords, postgres, database, sql
;;; Commentary:
;;
;; This package provides a few functions to make working with pg.el, 1password, org-mode, and others
;; It's probably not useful for anyone but me yet...
;;; Code:

(require 'eros)
(require 'one-pass)
(require 'pg-glue-connect)
(require 'pg-glue-query)
(require 'pg-glue-metadata)
(require 'pg-glue-utils)
(require 'pg-glue-view)

;;;;;;;;;;;
;;; connect

;;;###autoload
(defun pg-glue/connect (one-pass-item)
  "Connect to the database specified by ONE-PASS-ITEM."
  (interactive
   (list
    (completing-read
     "db: " (one-pass/list-items :categories '("database")) nil t)))
  (cl-destructuring-bind
	(&key database server username password port &allow-other-keys)
      (one-pass/get-item one-pass-item)
    (pg-glue-connect/set! database username password server port t)
    (pg-glue-metadata/set! pg-glue/connection)))

(defun pg-glue--ensure-connected ()
  "Ensure that there is a database connection open and create on if not."
  (unless pg-glue/connection
    (pg-glue/connect
     (completing-read
      "db: " (one-pass/list-items :categories '("database")) nil t))))

;;;;;;;;;
;;; query

;;;###autoload
(defun pg-glue/query (query-string)
  "Run a QUERY-STRING against the current connection."
  (interactive
   (progn
     (pg-glue--ensure-connected)
     (list (read-string "query: "))))
  (pg-glue-query/query pg-glue/connection query-string))

;;;###autoload
(defun pg-glue/query-paragraph ()
  "Run the query at point against the current database.
Optionally run the query displaying FORMAT.  The default is plist."
  (interactive)
  (pg-glue--ensure-connected)
  (save-excursion
    (let* ((start (progn (backward-paragraph) (point)))
	   (end (progn (forward-paragraph) (point)))
	   (query (buffer-substring-no-properties start end)))
      (eros--eval-overlay (pg-glue/query query) end))))

;;;;;;;;;;;;
;;; metadata

;;;###autoload
(defun pg-glue/metadata-refresh ()
  "Refresh the metadata for the current connection."
  (interactive)
  (pg-glue-metadata/set! pg-glue/connection))

(defmacro with-pg-glue-buffer (buffer-name &rest body)
  "Display insert data from BODY in BUFFER-NAME.
Like WITH-OUTPUT-TO-TEMP-BUFFER but uses INSERT instead of PRINC for
buffer content."
  (let ((buffer-var (gensym "buffer")))
    `(let (,buffer-var)
       (with-current-buffer (get-buffer-create ,buffer-name)
	 (setq buffer-read-only nil
	       ,buffer-var (current-buffer)
	       buffer-file-name nil
	       buffer-undo-list t)
	 (let ((inhibit-read-only t)
	       (inhibit-modification-hooks t))
	   (erase-buffer)
	   (run-hooks 'temp-buffer-setup-hook)
	   (progn ,@body)
	   ;; abstraction violation, but I can't find the function I want
	   (internal-temp-output-buffer-show ,buffer-var)
	   (select-window (get-buffer-window ,buffer-name)))))))

;;;###autoload
(defun pg-glue/tables (schema)
  "Show the tables for SCHEMA at the current connection."
  (interactive
   (progn
     (pg-glue--ensure-connected)
     (list
      (completing-read "schema: " (pg-glue-metadata/schemas) nil t))))
  (pg-glue--ensure-connected)
  (let ((header (format "Tables: %s" schema))
	(buffer-name (format "pg-glue: %s tables" schema)))
    (with-pg-glue-buffer buffer-name
      (insert
       (string-join
	(append
	 (list header
	       (pg-glue-view/colorize
		(make-string (length header) ?\=) :light-blue))
	 (pg-glue-metadata/tables schema))
	"\n")))))

;;;###autoload
(defun pg-glue/columns (schema table)
  "Show the columns for TABLE in SCHEMA."
  (interactive
   (let* ((select-schema (completing-read
			  "schema: " (pg-glue-metadata/schemas) nil t))
	  (select-table (completing-read
			 "table: "  (pg-glue-metadata/tables select-schema) nil t)))
     (pg-glue--ensure-connected)
     (list select-schema select-table)))
  (let* ((header (format "Table: %s.%s" schema table))
	 (buffer-name (format "%s: columns" table))
	 (columns (pg-glue-metadata/columns schema table))
	 (maxes (pg-glue-view/key-maxes columns))
	 (divider (pg-glue-view/colorize
		   (make-string (apply #'+ (pg-glue-utils/vals maxes)) ?\=)
		   :light-blue)))
    (with-pg-glue-buffer buffer-name
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
	"\n")))))

(provide 'pg-glue)
;;; pg-glue.el ends here
