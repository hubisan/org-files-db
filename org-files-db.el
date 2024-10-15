;;; org-files-db.el --- Add headings and links of org file to SQLite db -*- lexical-binding: t -*-

;; Copyright (C) 2024 Daniel Hubmann <hubisan@gmail.com>

;; Author: Daniel Hubmann <hubisan@gmail.com>
;; Maintainer: Daniel Hubmann <hubisan@gmail.com>
;; URL: https://github.com/hubisan/org-files-db
;; Keywords: outlines
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.8"))

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

;; Store headings and links from org files from one or more directories/files in
;; a SQLite database.

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

;;; Code:

;; Use this to add the files to the load path:
;; (let ((default-directory "~/projects/coding/org-files-db"))
;;   (normal-top-level-add-to-load-path '("."))
;;   (normal-top-level-add-subdirs-to-load-path))

;;;; * Requirements

(require 'subr-x)
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

(defun org-files-db-check-requirements ()
  "Check if the requirements are met to use `org-files-db'.
If the requirements are not met an `user-error' is signaled."
  (interactive)
  (let* ((sqlite-available (and (fboundp 'sqlite-available-p)
                                (sqlite-available-p)))
         (sqlite-executable (executable-find "sqlite3"))
         (ugrep-executable (executable-find "ugrep"))
         (fd-find-executable (executable-find org-files-db-fd-find-executable))
         (stat-executable (executable-find "stat"))
         (message
          (concat (unless sqlite-available
                    (concat
                     "\nEmacs doesn't have native SQLite support"
                     " -> Emacs 29 or higher is required"))
                  (unless sqlite-executable
                    (concat "\nexecutable 'sqlite3' not found"
                            " ->  install SQLite3"))
                  (unless ugrep-executable
                    (concat "\nexecutable 'ugrep' not found"
                            " -> install ugrep"))
                  (unless fd-find-executable
                    (concat
                     "\nexecutable '"
                     org-files-db-fd-find-executable
                     "' not found -> either fd-find needs to be installed"
                     " or the executable name has to be customized"))
                  (unless stat-executable
                    (concat "\nexecutable 'stat' not found"
                            " ->  install stat")))))
    (if (string-equal message "")
        "Requirements fulfilled"
      (user-error "Requirements not fulfilled, see the documentation:%s" message))))

;;;; * Parse Files

;; Uses ugrep (https://github.com/Genivia/ugrep) to parse the org files.

;;;; * File Management

;;;; * Auxiliary Functions

;;;; * Footer

(provide 'org-files-db)

;;; org-files-db.el ends here
