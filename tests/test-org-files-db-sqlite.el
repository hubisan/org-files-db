;;; test-org-files-db.el --- Tests for org-files-db -*- lexical-binding:t -*-

;;; Commentary:

;; Tests to check if the database functions are working.

;;; Code:

;;;; * Requirements

(require 'buttercup)
(require 'ert)

(require 'my-helpers)
(require 'org-files-db-core)
(require 'org-files-db-db)
(require 'org-files-db-sqlite)

;; Load helpers.
;; (let ((pkg-dir (file-name-directory (locate-library "org-files-db"))))
;;   (load (expand-file-name "tests/helpers.el" pkg-dir)))

;;;; * Helpers

;;;; * Build



(describe "org-files-db-sqlite"
  :var* ((pkg-dir (expand-file-name default-directory))
         (files-path (expand-file-name "tests/files" pkg-dir))
         ;; (schema-file (expand-file-name "schema.sql" files-path))
         (schema-file (expand-file-name "sql/db-schema.sql" org-files-db--load-dir))
         (schema-string (my-helpers-file-read-contents schema-file))
         (sqlite "sqlite3"))

  (describe "creates"
    :var* ((path-string (make-temp-file "test-string-" nil ".db"))
           (name-string "sqlite-from-string")
           (path-file (make-temp-file "test-file-" nil ".db"))
           (name-file "sqlite-from-file")
           (target-schema "CREATE TABLE directories (
    directory text NOT NULL PRIMARY KEY,
    updated integer NOT NULL,
    mtime integer NOT NULL,
    size integer NOT NULL
);")
           process-with-string
           process-with-file)
    (it "a database from SQL as string"
      (setq process-with-string
            (org-files-db--sqlite-create-database
             path-string schema-string name-string sqlite 1 t t 3000))
      (expect (file-exists-p path-string) :to-be t)
      (expect (processp (org-files-db--sqlite-process-get process-with-string)) :to-be t)
      (expect (process-name process-with-string) :to-equal name-string)
      (let ((schema (org-files-db--sqlite-execute
                     ".schema" 'list 10 process-with-string)))
        (expect (string-trim schema) :to-equal target-schema)))
    (it "a database from SQL in a file"
      (setq process-with-file
            (org-files-db--sqlite-create-database
             path-file schema-file name-file sqlite 1 t t 3000))
      (expect (file-exists-p path-file) :to-be t)
      (expect (processp (org-files-db--sqlite-process-get process-with-file)) :to-be t)
      (expect (process-name process-with-file) :to-equal name-file)
      (let ((schema (org-files-db--sqlite-execute
                     ".schema" 'list 10 process-with-file)))
        (expect (string-trim schema) :to-equal target-schema))))

  (describe "it"
    :var* ((path-main (make-temp-file "test-main-" nil ".db"))
           (name-main "sqlite-main")
           process
           process-restarted)
    (before-all
      (setq process
            (org-files-db--sqlite-create-database
             path-main schema-string name-main sqlite 1 t t 3000))
      (org-files-db--sqlite-execute
       "INSERT INTO directories VALUES('dir', 1, 1, 1);" nil nil process))
    (it "throws if the file would be overwritten and force is nil"
      (should-error
       (org-files-db--sqlite-create-database
        path-main schema-file name-main 1 sqlite nil t 3000)))
    (it "has set the correct user version"
      (expect (org-files-db--sqlite-get-user-version process) :to-be 1))
    (it "ignores the output of a SQL statement"
      (expect (org-files-db--sqlite-execute
               "SELECT * FROM directories;" nil nil process)
              :to-equal nil)
      (expect org-files-db--sqlite-output :to-be nil))
    (it "captures the output of a SQL statement with the mode used"
      (expect (string-trim (org-files-db--sqlite-execute
                            "SELECT * FROM directories;" 'list 1 process))
              :to-equal "dir|1|1|1")
      (expect (string-trim (org-files-db--sqlite-execute
                            "SELECT * FROM directories;" 'json 1 process))
            :to-equal "[{\"directory\":\"dir\",\"updated\":1,\"mtime\":1,\"size\":1}]")
      (expect (string-trim (org-files-db--sqlite-execute
                            "SELECT * FROM directories;" 'csv 1 process))
            :to-equal "dir,1,1,1"))
    (it "can parse the json output into a Lisp object"
        (expect (gethash "dir"
                         (org-files-db--sqlite-output-to-json
                          (org-files-db--sqlite-execute
                           "SELECT json_group_object(directory, mtime) FROM directories;"
                           'list nil process)))
                :to-equal 1))
    (it "shows error if needed"
      (spy-on 'org-files-db--sqlite-process-filter-check-for-error)
      (expect (org-files-db--sqlite-execute "SELECT * FROM notexisting;" nil nil process) :to-be nil)
      ;; Need to wait for the function to be called.
      (expect (progn (sleep-for 1)
                     (string-prefix-p
                      "Error: "
                      (nth 1 (spy-calls-args-for 'org-files-db--sqlite-process-filter-check-for-error 0))))
              :to-be t))
    ;; (it "shows a message if the status of the process has changed")
    (it "can set the default process"
      (org-files-db--sqlite-process-set-default process)
      (expect (org-files-db--sqlite-process-get) :to-be process))
    (it "makes the process current"
      (setq org-files-db--sqlite-process nil)
      (expect (org-files-db--sqlite-with process (org-files-db--sqlite-process-get) :to-be process)))
    (it "reuses the process if force is not used"
      (org-files-db--sqlite-process-start path-main name-main sqlite nil)
      (expect (process-live-p process) :not :to-be nil))
    (it "restarts the process if force is used"
      (spy-on #'org-files-db--sqlite-process-delete :and-call-through)
      (let ((kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                               kill-buffer-query-functions)))
        (setq process-restarted
              (org-files-db--sqlite-process-start path-main name-main sqlite t)))
      (expect 'org-files-db--sqlite-process-delete :to-have-been-called)
      (expect (process-live-p process) :to-be nil)
      (expect (processp (org-files-db--sqlite-process-get process-restarted)) :to-be t))
    (it "deletes the process and buffer"
      (expect (let ((kill-buffer-query-functions
                     (remq 'process-kill-buffer-query-function
                           kill-buffer-query-functions)))
                (org-files-db--sqlite-process-delete process-restarted t)
                (process-live-p process-restarted))
              :to-be nil)
      (expect (buffer-live-p (process-buffer process-restarted)) :to-be nil))
    (it "if terminated abruptly calls the sentinel"
      (spy-on 'org-files-db--sqlite-process-sentinel-handle-status-change :and-call-through)
      (spy-on 'message)
      (let ((kill-buffer-query-functions
             (remq 'process-kill-buffer-query-function
                   kill-buffer-query-functions)))
        (setq process (org-files-db--sqlite-process-start path-main name-main sqlite))
        (process-send-eof process))
      ;; Wait for it to terminate.
      (sleep-for 1)
      (expect 'org-files-db--sqlite-process-sentinel-handle-status-change :to-have-been-called)
      (expect 'message :to-have-been-called-with "Process '%s' has been deleted" name-main))))

(provide 'test-org-files-db)

;;; test-org-files-db-sqlite.el ends here
