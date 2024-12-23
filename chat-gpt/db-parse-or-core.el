;;;; * Parse Files

(defun org-files-db-parse-files ()
  "Parse all Org files specified in `org-files-db-source-paths`."
  (dolist (path org-files-db-source-paths)
    (if (file-directory-p path)
        (org-files-db-parse-directory path)
      (org-files-db-parse-file path))))

(defun org-files-db-parse-directory (directory)
  "Parse all Org files in DIRECTORY."
  (dolist (file (directory-files-recursively directory "\\.org$"))
    (unless (org-files-db-file-excluded-p file)
      (org-files-db-parse-file file))))

(defun org-files-db-file-excluded-p (file)
  "Check if FILE matches any pattern in `org-files-db-files-exclude-regexps`."
  (seq-some (lambda (regexp) (string-match-p regexp file))
            org-files-db-files-exclude-regexps))

(defun org-files-db-parse-file (file)
  "Parse a single Org FILE and insert data into the database."
  (let ((parsed-data (org-files-db-parse--parse-file file)))
    (org-files-db-database--insert-file-data file parsed-data)))

;;;; * Write Data to Database

(defun org-files-db-database--insert-file-data (file parsed-data)
  "Insert PARSED-DATA from FILE into the database."
  (let ((file-id (org-files-db-database--insert-file file)))
    (dolist (heading (car parsed-data))
      (org-files-db-database--insert-heading file-id heading))
    (dolist (link (cadr parsed-data))
      (org-files-db-database--insert-link file-id link))))

;;;; * Check for Modifications

(defun org-files-db-check-modifications ()
  "Check for modifications in Org files and update the database."
  (run-at-time nil org-files-db-check-interval 'org-files-db-parse-files))

;;;; * Async

(defun org-files-db-async-parse-files ()
  "Parse Org files asynchronously."
  (async-start
   `(lambda ()
      (setq org-todo-keywords ',org-todo-keywords
            org-inhibit-startup t
            org-agenda-files nil)
      (require 'org-files-db)
      (org-files-db-parse-files))
   (lambda (result)
     (message "Async parsing finished with result: %s" result))))

;;;; * Footer

(provide 'org-files-db-core)

;;; org-files-db-core.el ends here