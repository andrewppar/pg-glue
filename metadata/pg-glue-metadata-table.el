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
    "  , pg_catalog.format_type(a.atttypid , a.atttypmod) type"
    "  , pg_get_expr(d.adbin , d.adrelid) default"
    "from"
    "  pg_class c"
    "  join pg_namespace n on c.relnamespace = n.oid"
    "  join pg_attribute a on a.attrelid = c.oid"
    "  left join pg_catalog.pg_attrdef d on a.attrelid = d.adrelid"
    "    and a.attnum = d.adnum"
    "where"
    "  (relkind = 'r' or relkind = 'p')"
    (format "  and n.nspname = '%s'" schema)
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
	    (type (pg-glue-utils/get row "type"))
	    (default (pg-glue-utils/get row "default"))
	    (column-base (list "name" attname "number" attnum "type" type))
	    (column (if default
			(pg-glue-utils/assoc column-base "default" default)
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
