;;; pg-glue-query.el --- Glue code for postgres, pg.el, and some other tools -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andrew Parisi

;;; Commentary:
;;
;; Execute queries against a database connection
;;
;;; Code:
(require 'pg)

(defun pg-glue-query/query (db query-string &optional output-format)
  "Execute QUERY-STRING against DB.

OUTPUT-FORMAT specifies the way result rows are formatted.  Options are
table: output columns and rows of the query as a single list
alist: output rows of the query as alists whose keys are columns of the query.
plist: output rows of the query as plists whose keys are columns of the query.
The default is plist

Example:
`(pg-glue-query/query *DB* \"select * from exapmle.table\" \\'alist)`
===>
`(((\"one\" . 1) (\"two\" . 2)) ((\"one\" . 4) (\"two\" . 3)))`"
  (let* ((query-result (pg-exec db query-string))
	 (columns (pg-result query-result :attributes))
	 (column-names (mapcar #'car columns))
	 (rows (pg-result query-result :tuples)))
    (if (equal output-format 'table)
	(cons column-names rows)
      (mapcar
       (lambda (row)
	 (seq-reduce
	  (lambda (new-row idx)
	    (let* ((col (nth idx columns))
		   (column-name (car col))
		   (type (cadr col))
		   (raw-val (nth idx row))
		   (val (cond ((= type 1184) (decode-time raw-val "UTC0"))
			      ((= type 1114) (decode-time raw-val "UTC0"))
			      (t raw-val))))
	      (if (equal output-format 'alist)
		  (cons (cons col val) new-row)
		(plist-put new-row column-name val #'equal))))
	  (number-sequence 0 (- (length row) 1))
	  '()))
       rows))))

(provide 'pg-glue-query)
;;; pg-glue-query.el ends here
