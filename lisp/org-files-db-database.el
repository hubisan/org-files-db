;;; org-files-db-database.el --- Database access and storage -*- lexical-binding: t -*-

;; Copyright (C) 2025 Daniel Hubmann

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

;;; Code:

;;; Requirements

(require 'org-files-db-core)
(require 'sqlite)

;;; Constants & Variables

(defconst org-files-db-database--version 1
  "")

(defvar org-files-db-database--connection nil
  "The active SQLite database connection for org-files-db.")

(defvar org-files-db-database--schema-file nil
  "Path to the SQL schema file used to initialize the database.")

;;; Utility

;;; Connection

(defun org-files-db-database--open (path)
  "Open or create the Org files database at PATH."
  ;; make sure to check the version
  ;; make sure to turn fks on
  )

(defun org-files-db-database--close ()
  "Close the active Org files database connection."
  )

(defun org-files-db--ensure--connection ()
  "Ensure that `org-files-db-database--connection` is active."
  )

;;;; Initialize

(defun org-files-db-database--init ()
  "Initialize the Org files database schema."
  ;; dont forget to set the version.
  )

(defun org-files-db-database--execute-schema (schema-file)
  "Execute SQL commands from SCHEMA-FILE to initialize the database."
  )

;; If the db version doesn't match the current it needs to be reinitalized.
(defun org-files-db-database--check-version ()
  ""
  )

;;; Insert, Update & Delete

(defun org-files-db-database-insert-entry (entry)
  "Insert ENTRY (an alist) into the database."
  )

(defun org-files-db-database-update-entry (entry)
  "Update ENTRY in the database."
  )

(defun org-files-db-database-delete-entry (id)
  "Delete entry with ID from the database."
  )

;;; Query

(defun org-files-db-database-query (sql &optional params)
  "Execute SQL query with optional PARAMS and return results."
  )

;;; Provide

(provide 'org-files-db-database)

;;; org-files-db-database.el ends here
