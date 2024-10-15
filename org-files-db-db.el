;;; org-files-db-db.el --- Handle interaction with SQLite -*- lexical-binding: t -*-

;; Copyright (C) 2024 Daniel Hubmann

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Handles interactions with SQLite (e.g., creation of the database schema,
;; inserting and updating records).

;;; Code:

;;;; * Requirements

(require 'org-files-db-core)

;;;; * Constants & Variables

;; TODO Check old version of this to handle this better.
;; It is probably a defcustom.
(defvar org-files-db-file
  (expand-file-name "org-files.db" user-emacs-directory)
  "Path to the SQLite database file.")

(defconst org-files-db-db--schema-file
  (expand-file-name "sql/db-schema.sql" org-files-db--load-dir)
  "File with the schema to build the database.")

(defconst org-files-db-db--user-version 1
  "The current version of the database.
If the database version changes it will be rebuilt from scratch.")

(defvar org-files-db-db--connection nil
  "Database connection to the SQLite database.")

;;;; * Build

;; TODO Name is not so good, rather use initialize or so, Check older versions.
(defun org-files-db--build ()
  "Create the necessary tables in the SQLite database."
  (let ((db (org-files-db--open))
        (sql-statements '))
    (dolist (sql sql-statements)
      (sqlite-exec org-files-db--db-connection sql))
    (sqlite-pragma db (format "user_version=%s" org-files-db-db--user-version))
    ))

;;;; * Open & Close

(defun org-files-db--open (db-file)
  "Open a connection to the SQLite database."
  ;; TODO if file doesn't exist or the version doesn't match then build the
  ;; database from scratch.
  (let* ((db (unless org-files-db--db-connection
               ;; `sqlite-open' automatically creates the file if it doesn't
               ;; exist.
               (setq org-files-db--db-connection (sqlite-open db-file)))))
    (sqlite-pragma db "journal_mode=WAL")
    (sqlite-pragma db "foreign_keys=ON")))

(defun org-files-db--close ()
  "Close the connection to the SQLite database."
  (when org-files-db--db-connection
    (sqlite-close org-files-db--db-connection)
    (setq org-files-db--db-connection nil)))

(defun org-files-db-check-version (db)
  "Check the version of the database schema and recreate if necessary."
  (let* ((current-version (caar (sqlite-select db "PRAGMA user_version;")))
        (version-match (= current-version org-files-db-version)))
    (unless version-match
      (message "Database version mismatch. Recreating database..."))
    version-match))

;;;; * Insert

(defun org-files-db--insert-file (file-path)
  "Insert a FILE-PATH into the files table."
  (org-files-db--open-connection)
  (let ((timestamp (string-to-number (format-time-string "%s"))))
    (sqlite-exec org-files-db--db-connection
                 "INSERT INTO files (file_path, created_at, updated_at) VALUES (?, ?, ?);"
                 file-path timestamp timestamp)))

(defun org-files-db--insert-heading (file-id heading-text level scheduled-time deadline-time)
  "Insert a heading into the 'headings' table."
  (org-files-db--open-connection)
  (sqlite-exec org-files-db--db-connection
               "INSERT INTO headings (file_id, heading_text, level, scheduled_time, deadline_time)
                VALUES (?, ?, ?, ?, ?);"
               file-id heading-text level scheduled-time deadline-time))

(defun org-files-db-db--insert-tag (arg)
  ""
  )

(defun org-files-db-db--insert-property (arg)
  ""
  )

(defun org-files-db-db--insert-link (arg)
  ""
  )

;;;; * Update

;;;; * Delete

;;;; * Queries

;;;; * Footer

(provide 'org-files-db-db)

;;; org-files-db.el ends here
