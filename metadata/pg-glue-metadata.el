;;; pg-glue.el --- Glue code for postgres, pg.el, and some other tools -*- lexical-binding: t -*-

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
(require 'pg-glue-metadata-table)
(require 'pg-glue-metadata-constraint)
(require 'pg-glue-query)
(require 'pg-glue-utils)

(defvar pg-glue/metadata '()
  "Any metadata for a connection.")

(defun pg-glue-metadata--set! (db schema)
  "Generate a plist for the metadata of SCHEMA in DB."
  (pg-glue-metadata-constraint/constraints
   db schema (pg-glue-metadata-table/tables db schema)))

(defconst pg-glue-metadata-excluded-schemas
  (mapcar
   (lambda (schema)
     (format "'%s'" schema))
   (list "pglogical" "information_schema" "pg_catalog" "pg_toast"))
  "Schemas that are exluded from metadata capture.")

(defun pg-glue-metadata--schemas (db)
  "Get all schemas in DB."
  (let ((exclusions (string-join pg-glue-metadata-excluded-schemas ", ")))
    (mapcar
     #'cadr
     (pg-glue-query/query
      db (string-join
	  (list "select schema_name"
		"from information_schema.schemata"
		(format "where schema_name not in (%s)" exclusions))
	  "\n")))))

(defun pg-glue-metadata/set! (db)
  "Set any metadata for all the schemas of DB."
  (setq
   pg-glue/metadata
   (seq-reduce
    (lambda (metadata schema)
      (pg-glue-utils/assoc
       metadata schema (pg-glue-metadata--set! db schema)))
    (pg-glue-metadata--schemas db)
    '())))

(defun pg-glue-metadata/schemas ()
  "Get the SCHEMAS in PG-GLUE/METADATA."
  (sort
   (pg-glue-utils/keys pg-glue/metadata)
   :lessp #'string< :in-place t))

(defun pg-glue-metadata/tables (schema)
  "Get all tables in SCHEMA."
  (thread-first
    pg-glue/metadata
    (pg-glue-utils/get schema)
    pg-glue-utils/keys
    (sort :lessp #'string< :in-place t)))

(defun pg-glue-metadata/columns (schema table &optional just-names?)
  "Get all columns for TABLE in SCHEMA."
  (let ((columns (thread-first
		   pg-glue/metadata
		   (pg-glue-utils/get-in (list schema table "column"))
		   (sort :key (lambda (col) (pg-glue-utils/get col "number"))
			 :lessp #'<
			 :in-place t))))
    (if just-names?
	(mapcar (lambda (spec) (pg-glue-utils/get spec "name")) columns)
      columns)))

(defun pg-glue-metadata/primary-key (schema table)
  "Get the primary key for TABLE in SCHEMA."
  (pg-glue-utils/get-in pg-glue/metadata (list schema table "primary-key")))

(defun pg-glue-metadata/unique-constraints (schema table)
  "Get the unique constraints for TABLE in SCHEMA."
  (pg-glue-utils/get-in
   pg-glue/metadata (list schema table "unique-constraint")))

(defun pg-glue-metadata/foreign-keys (schema table)
  "Get the foreign and domestic keys for TABLE in SCHEMA."
  (list "foreign-key"
	(pg-glue-utils/get-in
	 pg-glue/metadata (list schema table "foreign-key"))
	"domestic-key"
	(pg-glue-utils/get-in
	 pg-glue/metadata (list schema table "domestic-key"))))

(provide 'pg-glue-metadata)
;;; pg-glue-metadata.el ends here
