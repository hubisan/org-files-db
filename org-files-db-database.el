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

(require 'org-files-db-sqlite)

;; * Variables

(defconst org-files-db--database-version 1
  "The version of the database.
If the database version changes an existing database with an older version will
be rebuilt from scratch.")

(defvar org-files-db--database-process nil
  "Process that runs the SQLite3 interative shell.")

(defconst org-files-db--database-process-name "org-files-db"
  "The name for the process. Is also used to name the buffer.")

;; * Create

;; TODO
;; Connect if the database already exists else create it and connect to it.
(defun org-files-db--database-connect (force)
  "docstring")

(defun org-files-db--database-disconnect ()
  "docstring")

;; TODO
(defun org-files-db--database-create (path schema-path)
  "Create the database at absolute PATH. If it already exists it is overwritten.
Use the schema found at `schema-path' to create the tables and indices."
  )

;; TODO
(defun org-files-db--database-exists-p (path)
  "Check if the database file with PATH exists."
  (file-exists-p path))

;; TODO
(defun org-files-db--database-create-db-file (path)
  "Create the database file at PATH.
Will overwrite existing files and creates parent directories if needed."
  (make-empty-file path t))

;; TODO
(defun org-files-db--database-set-user-version (db version)
  "Set the user_version to VERSION for the DB."
  (org-files-db--db-execute
   db (format "PRAGMA user_version = %s;" version)))

;; TODO
(defun org-files-db--database-get-user-version (db)
  "Get the user_version of the open DB connection."
  (plist-get
   (seq-first (org-files-db--db-execute-get-output
               db "PRAGMA user_version;" 'plist))
   :user_version))

;; * Connect

;; * Insert

;; * Update

;; * Delete

;; * Select

;; * Footer

(provide 'org-files-db-database)

;;; org-files-db-database.el ends here
