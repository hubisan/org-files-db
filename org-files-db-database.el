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

;; Functions to interact with the sqlite database.

;;; Code:

;;;; * Requirements

(require 'seq)
(require 'sqlite)

;;;; * Variables

;;;; * Open & Close

(defun org-files-db--database-open (file user-version schema-file)
  "Open the database FILE. If it doesn't exist it will be build.
If the USER-VERSION doesn't match the version stored in the
database, the database is rebuild. The SCHEMA-FILE is used to
build the database."
  (let* ((db-exists (org-files-db--database-exists-p file))
         (db (when db-exists (sqlite-open file))))
    (unless (and db-exists (= user-version (org-files-db--database-get-user-version db)))
      ;; Build or rebuid the database.
      (org-files-db--database-build file user-version schema-file))
    (org-files-db--database-turn-foreign-keys-on db)
    db))

(defun org-files-db--database-close (db)
  "Close the DB."
  (sqlite-close db))

;;;; * Build

(defun org-files-db--database-build (file user-version schema-file)
  "Build the database FILE using SCHEMA-FILE and set USER-VERSION.
Returns the sqlite database object of the created database."
  (org-files-db--database-create-file file)
  (let ((db (sqlite-open file)))
    (org-files-db--database-create-schema db schema-file)
    (org-files-db--database-set-user-version db user-version)))

(defun org-files-db--database-create-file (file)
  "Create the database FILE.
Will overwrite existing files and creates parent directories if needed."
  (make-empty-file file t))

(defun org-files-db--database-create-schema (db schema-file)
  "Create the database schema using SCHEMA-FILE."
  (sqlite-execute db ".read ?" (list (expand-file-name schema-file))))

;;;; * Auxiliary Functions

(defun org-files-db--database-exists-p (file)
  "Check if the database FILE exists."
  (file-exists-p file))

(defun org-files-db--database-set-user-version (db version)
  "Set the user_version to VERSION for the DB."
  (sqlite-pragma db (format "user_version = %s" version)))

(defun org-files-db--database-get-user-version (db)
  "Get the user_version of the DB."
  (caar (sqlite-select db "PRAGMA user_version")))

(defun org-files-db--database-turn-foreign-keys-on (db)
  "Turn foreign keys on for the database.
This has to be done on every new connection as SQLite is usually
compiled with foreign keys turned off by default."
  (sqlite-pragma db "foreign_keys = ON"))

;;;; * Footer

(provide 'org-files-db-database)

;;; org-files-db-database.el ends here
