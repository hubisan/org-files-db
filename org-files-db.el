;;; org-files-db.el --- Add headings and links of org file to SQLite db -*- lexical-binding: t -*-

;; Copyright (C) 2022 Daniel Hubmann <hubisan@gmail.com>

;; Author: Daniel Hubmann <hubisan@gmail.com>
;; Maintainer: Daniel Hubmann <hubisan@gmail.com>
;; URL: https://github.com/hubisan/org-files-db
;; Keywords: outlines
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.5") (emacsql-sqlite "1.0.0"))

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

;; Store headings and links from org files from one or more directories in a
;; SQLite database.

;;  Main features

;; - Store headings and links from org files from one or more directories in a
;;   SQLite database.
;; - For each heading it stores the filename, line number, title, level,
;;   priority, todo keyword, statistic cookies, planning info (scheduled,
;;   deadline, closed),
;;   parent heading id, tags and properties.
;; - For each link it stores the filename, position, full link, type, link and
;;   description.
;; - Provides queries for the most common use cases.
;; - Uses ugrep to quickly parse the files.
;; - Uses fd to check if files are modified for reparsing.

;;; Code:

;; Use this to add the files to the load path:
;; (let ((default-directory "~/projects/coding/org-files-db"))
;;   (normal-top-level-add-to-load-path '("."))
;;   (normal-top-level-add-subdirs-to-load-path))

;;;; * Requirements

(require 'seq)
(require 'sqlite)

(require 'org-files-db-database)
(require 'org-files-db-core)

;;;; * Constants & Variables

(defconst org-files-db--db-schema-file
  (expand-file-name "sql/db-schema.sql" org-files-db--load-dir)
  "File with the schema to build the database.")

(defconst org-files-db--db-user-version 1
  "The current version of the database.
If the database version changes it will be rebuilt from scratch.")

(defvar org-files-db--db-object nil
  "Holds the sqlite object.")

;;;; * TODO

(defun org-files-db-rename-file (args)
  "docstring"
  (interactive "P")

  )


;;;; * Initialize

(defun org-files-db--check-requirements ()
  "Check if the requirements are met to use `org-files-db'."
  ;; TODO Does this also check if the executable is available?
  (let* ((sqlite-availabe (sqlite-available-p))
         (sqlite-executable (executable-find "sqlite3")))))

;;;; * DB

;;;;; ** DB Open

(defun org-files-db--open ()
  "Open the main database and return the sqlite object.
If the database doesn't exist or the user-version has changed the
data is (re)built from scratch."
  (setq org-files-db--db-object
        (org-files-db--database-open
         org-files-db-file
         org-files-db--db-user-version
         org-files-db--db-schema-file)))

;;;;; ** DB Insert

(defun org-files-db--insert-directory (args)
  ""

  )

(defun org-files-db--insert-file (args)
  ""

  )

(defun org-files-db--insert-heading (args)
  ""

  )

(defun org-files-db--insert-tag (args)
  ""

  )

(defun org-files-db--insert-property (args)
  ""

  )

(defun org-files-db--insert-link (args)
  ""

  )

;;;;; ** DB Update

(defun org-files-db--update-directory (args)
  ""

  )

(defun org-files-db--update-file (args)
  ""

  )

(defun org-files-db--update-heading (args)
  ""

  )

(defun org-files-db--update-tag (args)
  ""

  )

(defun org-files-db--update-property (args)
  ""

  )

(defun org-files-db--update-link (args)
  ""

  )

;;;;; ** DB Delete

(defun org-files-db--delete-directory (args)
  ""

  )

(defun org-files-db--delete-file (args)
  ""

  )

(defun org-files-db--delete-heading (args)
  ""

  )

(defun org-files-db--delete-tag (args)
  ""

  )

(defun org-files-db--delete-property (args)
  ""

  )

(defun org-files-db--delete-link (args)
  ""

  )

;;;;; ** DB Queries

;;;; * Parse Files

;; Uses ugrep (https://github.com/Genivia/ugrep) to parse the org files.

;;;; * File Management

;;;; * Auxiliary Functions

(defun org-files-db--sqlite-available-p ()
  "Return t if sqlite3 support is available in this instance of Emacs."
  (and (fboundp 'sqlite-available-p) (sqlite-available-p)))

;;;; * Footer

(provide 'org-files-db)

;;; org-files-db.el ends here
