;;; org-files-db-database.el --- Interaction with the DB -*- lexical-binding: t -*-

;; Copyright (C) 2022 Daniel Hubmann

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

;; Variables and functions to interact with the database.

;;; Code:

;; * Requirements

(require 'seq)
(require 'sqlite)

(require 'org-files-db-core)

;; * Variables

(defvar org-files-db--database-object nil
  "Holds the sqlite object.")

(defconst org-files-db--database-schema-path
  (expand-file-name "sql/db-schema.sql" org-files-db--load-dir)
  "Path where the schema to create the database is stored.
This is relative to the directory the package is installed.")

(defconst org-files-db--database-user-version 1
  "The current version of the database.
If the database version changes it will be rebuilt from scratch.")

;; * Open & Close


(defun org-files-db--database-open ()
  "Open the database at PATH. If it doesn't exist it will be created.
Uses the path stored in the customizable variable `org-files-db-path'.
If the `org-files-db--database-version' doesn't match the version stored in the
database, the database is rebuild."
  (if (sqlitep org-files-db--database-object)
      ;; Just return the existing database object if it already is set.
      org-files-db--database-object
    ;; If the database exists and the version matches return the db object.
    ;; If the database exists but the version does't match rebuild it.
    ;; If the database doesn't exist create and build it.
    (let* ((path org-files-db-path)
           (version org-files-db--database-get-user-version)
           db version-matches)
      (when (org-files-db--database-exists-p path)
          (setq db (sqlite-open path))
          (if (sqlitep db)
              (setq version-matches (= (org-files- process)
                                 version)))

          )
      (when (org-files-db--database-exists-p path)
        (setq version-matches (= (org-files-db--sqlite-get-user-version process)
                                 version)))
      ;; Create or recreate the db if needed.
      (setq org-files-db--database-process
            (if (and process version-matches)
                process
              (cond ((not process) (message "Creating new database"))
                    ((not version-matches)
                     (message (concat "Recreating database as the "
                                      "version doesn't match"))))
              )))))

(defun org-files-db--database-close ()
  "Close the database."
  (sqlite-close org-files-db--database-object))

;; * Build

(defun org-files-db--database-create-file (path)
  "Create the database file at PATH.
Will overwrite existing files and creates parent directories if needed."
  (make-empty-file path t))

(defun org-files-db--database-create-schema (db schema)
  "Create the SCHEMA in the DB."
  (with-sqlite-transaction db
    ;; Build the database with the schema stored in the file.
    (sqlite-execute db schema)))

;; * Auxiliary Functions

(defun org-files-db--database-exists-p (path)
  "Check if the database file with PATH exists."
  (file-exists-p path))

(defun org-files-db--database-set-user-version (db version)
  "Set the user_version to VERSION for the DB."
  (sqlite-pragma db (format "user_version = %s;" version)))

(defun org-files-db--database-get-user-version (db)
  "Get the user_version of the DB."
  (caar (sqlite-select db "PRAGMA user_version;")))

(defun org-files-db--database-turn-foreign-keys-on (db)
  "Turn foreign keys on for the database.
This has to be done on every new connection as SQLite is usually
compiled with foreign keys turned off by default."
  (sqlite-pragma db "foreign_keys = ON;"))

;; * Insert

;; * Update

;; * Delete

;; * Select

;; * Footer

(provide 'org-files-db-database)

;;; org-files-db-database.el ends here
