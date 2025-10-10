;;; pg-glue-table-metadata-table.el --- Glue code for postgres, pg.el, and some other tools -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andrew Parisi

;;; Commentary:
;;
;;  Get any metadata about tables for a db
;;
;;; Code:
(require 'pg-glue-query)
(require 'pg-glue-utils)

(defun pg-glue-metadata-table--table-query (schema)
  "Query for fetching table metadata from SCHEMA."
  (string-join
   (list
    "select"
    "  relname"
    "  , c.oid"
    "  , a.attname"
    "  , a.attnum"
    "  , pg_catalog.format_type(a.atttypid , a.atttypmod) as type"
    "  , pg_get_expr(d.adbin , d.adrelid) as default"
    "  , a.attgenerated as generated_type"
    "from"
    "  pg_class c"
    "  join pg_namespace n on c.relnamespace = n.oid"
    "  join pg_attribute a on a.attrelid = c.oid"
    "  left join pg_catalog.pg_attrdef d on a.attrelid = d.adrelid"
    "    and a.attnum = d.adnum"
    "where"
    "  (relkind = 'r' or relkind = 'p')"
    (format "  and n.nspname = '%s'" schema)
    "  and not attisdropped"
    "  and attnum > 0")
   "\n"))

(defun pg-glue-metadata-table/tables (db schema)
  "Get all tables in SCHEMA for DB connection."
  (seq-reduce
   (lambda (result row)
     (let* ((relname (pg-glue-utils/get row "relname"))
	    (oid (pg-glue-utils/get row "oid"))
	    (attname (pg-glue-utils/get row "attname"))
	    (attnum (pg-glue-utils/get row "attnum"))
	    (generated (pcase (pg-glue-utils/get row "generated_type")
			 (115 "stored")
			 (_ nil)))
	    (type (pg-glue-utils/get row "type"))
	    (default (pg-glue-utils/get row "default"))
	    (column-base (pg-glue-utils/assoc
			  (list "name" attname "number" attnum "type" type)
			  "generated" generated))
	    (column (if default
			(pg-glue-utils/assoc
			 (pg-glue-utils/assoc column-base "default-raw" default)
			 "default"
			 (if generated
			     (format "generated always as %s" default)
			   default))
		      column-base)))
       (thread-first
	 result
	 (pg-glue-utils/assoc-in (list relname "oid") oid)
	 (pg-glue-utils/update-in (list relname "column")
				  (lambda (columns) (cons column columns))))))
   (pg-glue-query/query db (pg-glue-metadata-table--table-query schema))
   '()))

(provide 'pg-glue-metadata-table)
;;; pg-glue-metadata-table.el ends here
