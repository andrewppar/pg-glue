;;; pg-glue-connect.el --- Glue code for postgres, pg.el, and some other tools -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andrew Parisi

;;; Commentary:
;;
;; Generate and maintain database connections
;;
;;; Code:
(require 'pg)

(defvar pg-glue/connection nil
  "The database connection object.")

(defun pg-glue-connect/set!
    (database username password server port &optional ssl?)
  "Set the connection with DATABASE, USERNAME, PASSWORD, SERVER and PORT.
Optionally pass SSL? to use an ssl connection."
  (when pg-glue/connection
    (pg-disconnect pg-glue/connection))
  (setq pg-glue/connection
	(pg-connect database username password server port ssl?)))

(provide 'pg-glue-connect)
;;; pg-glue-connect.el ends here
