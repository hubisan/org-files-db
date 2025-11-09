;;; org-files-db-database.el --- Handle interaction with SQLite -*- lexical-binding: t -*-

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
;; inserting and updating records). It is part of the org-files-db package,
;; which stores Org file data in a SQLite database.

;;; Code:

;;;; * Requirements

(require 'org-files-db-core)
(require 'sqlite)

;;;; * Global Constants & Variables

(defconst org-files-db-database--schema-file
  (expand-file-name "sql/db-schema.sql" org-files-db--install-directory)
  "File with the schema to build the database.")

(defconst org-files-db-database--user-version 1
  "The current version of the database.
If the database version changes it will be rebuilt from scratch.")

(defvar org-files-db-database--connection nil
  "Database connection to the SQLite database.")

;;;; * Initialize

(defun org-files-db-database--initialize (db-filename)
  "Initialize the SQLite database and create the necessary tables.
Database will be stored at DB-FILENAME."
  (let ((db (sqlite-open db-filename))
        (sql-statements '("bla")))
    (dolist (sql sql-statements)
      (sqlite-execute org-files-db-database--connection sql))
    (org-files-db--sqlite-set-user-version db org-files-db-database--user-version)))

;;;; * Open & Close

(defun org-files-db-database--open ()
  "Open a connection to the SQLite database."
  (if (sqlitep org-files-db-database--connection)
      org-files-db-database--connection
    (let* ((db-filename org-files-db-database-file))
      (if (not (org-files-db-database--database-file-exists))
          (progn
            (message "Org-files-db: Aborting. Database doesn't exist.
Database needs to be built from scratch.
You will be notified once the database has been built in the background.")
            (org-files-db-database--initialize db-filename)
            nil)
        (let* ((db (sqlite-open db-filename)))
          (if (not (org-files-db-database--check-version db))
              (progn
                (message "Org-files-db: Aborting. Database version mismatch.
Recreating database. You will be notified once the database has been rebuilt.")
                (org-files-db-database--close db)
                (org-files-db-database--initialize db-filename)
                nil)
            (sqlite-pragma db "journal_mode=WAL")
            (sqlite-pragma db "foreign_keys=ON")
            (setq org-files-db-database--connection db)))))))

(defun org-files-db-database--close (&optional db-connection)
  "Close the connection to the SQLite database.
If DB-CONNECTION is not given it uses the global variable
`org-files-db-database--connection'."
  (let* ((db-connection (or db-connection org-files-db-database--connection)))
    (when (sqlitep db-connection)
      (sqlite-close db-connection))
    (setq org-files-db-database--connection nil)))

(defun org-files-db-database--database-file-exists ()
  "Check if the database file  exists. "
  (file-exists-p org-files-db-database-file))

(defun org-files-db--sqlite-create-schema (db schema-file)
  "Create the database schema using SCHEMA-FILE."
  (sqlite-execute db ".read ?" (list (expand-file-name schema-file))))

(defun org-files-db--sqlite-set-user-version (db version)
  "Set the user_version to VERSION for the DB."
  (sqlite-pragma db (format "user_version = %s" version)))

(defun org-files-db--sqlite-get-user-version (db)
  "Get the user_version of the DB."
  (caar (sqlite-select db "PRAGMA user_version")))

(defun org-files-db-database--check-version (db)
  "Check if the version of the database matches the current version.
The current version is stored in `org-files-db-database--user-version'."
  (let* ((current-version (org-files-db--sqlite-get-user-version db)))
    (= current-version org-files-db-database--user-version)))

(defun org-files-db--sqlite-turn-foreign-keys-on (db)
  "Turn foreign keys on for the database.
This has to be done on every new connection as SQLite is usually
compiled with foreign keys turned off by default."
  (sqlite-pragma db "foreign_keys = ON"))

;;;; * Insert

(defun org-files-db--insert-file ()
  ""
  )

(defun org-files-db--insert-heading ()
  ""
  )

(defun org-files-db-database--insert-tag ()
  ""
  )

(defun org-files-db-database--insert-property ()
  ""
  )

(defun org-files-db-database--insert-link ()
  ""
  )

;;;; * Update

;;;; * Delete

;;;; * Queries

;;;; * Footer

(provide 'org-files-db-database)

;;; org-files-db.el ends here
