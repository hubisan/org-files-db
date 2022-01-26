;;; test-org-files-db.el --- Tests for org-files-db -*- lexical-binding:t -*-

;;; Commentary:

;; Tests to check if the database functions are working.

;;; Code:

;; * Requirements

(require 'buttercup)
(require 'ert)

(require 'helpers)
(require 'org-files-db-sqlite)

;; Load helpers.
;; (let ((pkg-dir (file-name-directory (locate-library "org-files-db"))))
;;   (load (expand-file-name "tests/helpers.el" pkg-dir)))

;; * Helpers


;; * Build

(describe "Sqlite DB"
  :var* ((pkg-dir (expand-file-name default-directory))
         (files-path (expand-file-name "tests/files" pkg-dir))
         (tmp-path (expand-file-name temporary-file-directory))
         (schema-file (expand-file-name "schema.sql" files-path))
         (schema-string (helpers-file-read-contents schema-file))
         (path-string (expand-file-name "test-string.db" tmp-path))
         (path-file (expand-file-name "test-file.db" tmp-path))
         (name-string "sqlite-from-string")
         (name-file "sqlite-from-file")
         (schema )
         ;; don't ask to kill buffers with a live process attached

         process-with-string
         process-with-file
         schema)

  (before-all
    (setq schema ))
  (describe "creates"
    (it "DB and process from SQL as string"
      (setq process-with-string
            (org-files-db--sqlite-create-database
             path-string schema-string name-string 1 t t 3000))

      (expect (processp (org-files-db--sqlite-process-get process-with-string)) :to-be t)
      (expect (process-name process-with-string) :to-equal name-string)
      (let ((schema (org-files-db--sqlite-execute
                     ".schema" 'list 10 process-with-string)))
        (expect schema :to-equal "CREATE TABLE directories (
    directory text NOT NULL PRIMARY KEY,
    updated integer NOT NULL,
    mtime integer NOT NULL,
    size integer NOT NULL
);")))
    (it "DB and process from SQL in a file"
      (setq process-with-file
            (org-files-db--sqlite-create-database
             path-file schema-file name-file 1 t t 3000))

      (expect (processp (org-files-db--sqlite-process-get process-with-file)) :to-be t)
      (expect (process-name process-with-file) :to-equal name-file)
      (let ((schema (org-files-db--sqlite-execute
                     ".schema" 'list 10 process-with-file)))
        (expect schema :to-equal "CREATE TABLE directories (
    directory text NOT NULL PRIMARY KEY,
    updated integer NOT NULL,
    mtime integer NOT NULL,
    size integer NOT NULL
);"))))

  (describe "generally"
    (it "throws if the file would be overwritten"
      (should-error
       (org-files-db--sqlite-create-database
        path-string schema-file name-string
        1 nil t 3000)))
    (it "has set the correct user version"
      (expect (org-files-db--sqlite-get-user-version process-with-string) :to-be 1))
    (it "ignores the output of a SQL statement")
    (it "captures the output of a SQL statement")
    (it "can parse the output of a SQL statement as json")
    (it "shows an user-error if there was an error in a SQL statement")
    (it "shows a message if the status of the process has changed")
    (it "can set the default process"
      (org-files-db--sqlite-process-set-default process-with-string)
      (expect (org-files-db--sqlite-process-get) :to-be process-with-string))
    (it "makes the process current"
      (expect (org-files-db--sqlite-with process-with-file
                (org-files-db--sqlite-process-get) :to-be process-with-file)))
    (it "reuses the process if force is not used"
      (org-files-db--sqlite-process-start path-string name-string nil)
      (expect (process-live-p process-with-string) :not :to-be nil))
    (it "restarts the process if force is used"
      (spy-on #'org-files-db--sqlite-process-delete :and-call-through)
      (let ((kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                               kill-buffer-query-functions)))
        (org-files-db--sqlite-process-start path-string name-string t))
      (expect 'org-files-db--sqlite-process-delete :to-have-been-called)
      (expect (process-live-p process-with-string) :to-be nil))
    (it "deletes the process and buffer"
      (expect (let ((kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                                       kill-buffer-query-functions)))
                (org-files-db--sqlite-process-delete process-with-file t)
                (process-live-p process-with-file))
              :to-be nil)
      (expect (buffer-live-p (process-buffer process-with-file))
              :to-be nil))))

(provide 'test-org-files-db)

;;; test-org-files-db.el ends here
