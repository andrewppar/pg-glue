;;; pg-glue-metadata-constraint.el --- Glue code for postgres, pg.el, and some other tools -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andrew Parisi

;;; Commentary:
;;
;; Add constraints to the metadata
;;
;;; Code:
(require 'pg-glue-query)
(require 'pg-glue-utils)
(require 'subr-x)

(defun pg-glue-metadata-constraint--oid->table (metadata oid)
  "Find the name of the table with OID in SCHEMA in METADATA."
  (pg-glue-utils/do-plist
   metadata
   (lambda (_result key val)
     (when (equal (pg-glue-utils/get val "oid") oid)
       key))
   #'identity))

(defun pg-glue-metadata-constraint--column-number->name (columns number)
  "Convert a column NUMBER to a column NAME from COLUMNS."
  (seq-some
   (lambda (column)
     (when (equal (pg-glue-utils/get column "number") number)
       (pg-glue-utils/get column "name")))
   columns))

(defun pg-glue-metadata-constraint--columns-for-key
    (metadata key-info foreign?)
  "Get the columns from KEY-INFO in METADATA.
FOREIGN? specifies which columns."
  (let* ((table (pg-glue-metadata-constraint--oid->table
		 metadata
		 (pg-glue-utils/get
		  key-info (if foreign? "confrelid" "conrelid"))))
	 (column-spec (thread-first
			metadata
			(pg-glue-utils/get table)
			(pg-glue-utils/get "column"))))
    (mapcar
     (lambda (id)
       (pg-glue-metadata-constraint--column-number->name column-spec id))
     (pg-glue-utils/get key-info (if foreign? "confkey" "conkey")))))

(defun pg-glue-metadata-constraint--add-foreign-key
    (metadata foreign-key-info)
  "Add FOREIGN-KEY-INFO to METADATA.
Data is added to both the foreign and the domestic tables in METADATA."
  (let ((name (pg-glue-utils/get foreign-key-info "conname"))
	(domestic-columns (pg-glue-metadata-constraint--columns-for-key
			   metadata foreign-key-info nil))
	(domestic-table (pg-glue-metadata-constraint--oid->table
			 metadata (pg-glue-utils/get foreign-key-info "conrelid")))
	(foreign-columns (pg-glue-metadata-constraint--columns-for-key
			  metadata foreign-key-info t))
	(foreign-table (pg-glue-metadata-constraint--oid->table
			metadata (pg-glue-utils/get foreign-key-info "confrelid")))
	(delete-rule (pg-glue-utils/get foreign-key-info "delete_rule")))
    (thread-first
      metadata
      (pg-glue-utils/update-in
       (list domestic-table "domestic-key")
       (lambda (keys)
	 (cons
	  (list
	   "name" name "columns" domestic-columns
	   "delete-rule" delete-rule
	   "foreign-table" foreign-table "foreign-columns" foreign-columns)
	  keys)))
      (pg-glue-utils/update-in
       (list foreign-table "foreign-key")
       (lambda (keys)
	 (cons
	  (list
	   "name" name "columns" foreign-columns
	   "delete-rule" delete-rule
	   "domestic-table" domestic-table "domestic-columns" domestic-columns)
	  keys))))))

(defun pg-glue-metadata-constraint--add-unary-table-constraint
    (metadata constraint-info constraint-type)
  "Add CONSTRAINT-INFO as CONSTRAINT-TYPE to METADATA."
  (let* ((name (pg-glue-utils/get constraint-info "conname"))
	 (columns (pg-glue-metadata-constraint--columns-for-key
		   metadata constraint-info nil))
	 (table (pg-glue-metadata-constraint--oid->table
		 metadata
		 (pg-glue-utils/get constraint-info "conrelid")))
	 (location (list table constraint-type))
	 (constraint (list "name" name "columns" columns)))
    (if (equal constraint-type "primary-key")
	(pg-glue-utils/assoc-in metadata location constraint)
      (pg-glue-utils/update-in
       metadata location (lambda (keys) (cons constraint keys))))))

(defun pg-glue-metadata-constraint--add-primary-key
    (metadata constraint-info)
  "Add a primary key constraint into METADATA with CONSTRAINT-INFO."
  (pg-glue-metadata-constraint--add-unary-table-constraint
   metadata constraint-info "primary-key"))

(defun pg-glue-metadata-constraint--add-unique-constraint
    (metadata constraint-info)
  "Add a unique constraint into METADATA with CONSTRAINT-INFO."
  (pg-glue-metadata-constraint--add-unary-table-constraint
   metadata constraint-info "unique-constraint"))

(defun pg-glue-metadata-constraint--constraint-query (schema)
  "Create a query for the constraints in SCHEMA."
  (string-join
   (list
    "select"
    "  c.conname"
    "  , c.contype"
    "  , c.conrelid"
    "  , c.confrelid"
    "  , c.conkey"
    "  , c.confkey"
    "  , rc.delete_rule"
    "from"
    "  pg_constraint c"
    "  join pg_namespace n on c.connamespace = n.oid"
    "  left join information_schema.referential_constraints rc"
    "    on rc.constraint_name = c.conname"
    "where"
    (format "  n.nspname = '%s'" schema)
    "  and c.contype in ('f' , 'u' , 'p')")
   "\n"))

(defun pg-glue-metadata-constraint/constraints (db schema metadata)
  "Get constraints from DB for SCHEMA storing in METADATA."
  (seq-reduce
   (lambda (result row)
     (cl-case (pg-glue-utils/get row "contype")
       (112 (pg-glue-metadata-constraint--add-primary-key result row))
       (117 (pg-glue-metadata-constraint--add-unique-constraint result row))
       (102 (pg-glue-metadata-constraint--add-foreign-key result row))))
   (pg-glue-query/query db (pg-glue-metadata-constraint--constraint-query schema))
   metadata))


(provide 'pg-glue-metadata-constraint)
;;; pg-glue-metadata-constraint.el ends here
