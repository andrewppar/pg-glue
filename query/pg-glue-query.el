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
	 (columns (mapcar #'car (pg-result query-result :attributes)))
	 (rows (pg-result query-result :tuples)))
    (if (equal output-format 'table)
	(cons columns rows)
      (mapcar
       (lambda (row)
	 (seq-reduce
	  (lambda (new-row idx)
	    (let ((col (nth idx columns))
		  (val (nth idx row)))
	      (if (equal output-format 'alist)
		  (cons (cons col val) new-row)
		(plist-put new-row col val #'equal))))
	  (number-sequence 0 (- (length row) 1))
	  '()))
       rows))))

(provide 'pg-glue-query)
;;; pg-glue-query.el ends here
