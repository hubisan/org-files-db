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

;; * Requirements

(require 'org-files-db-database)

;; * Customization

(defgroup org-files-db nil
  "Store headings and links from org files in a SQLite database."
  :group 'org
  :link '(url-link :tag "Github" "https://github.com/hubisan/org-files-db")
  :prefix "org-files-db-")

(defcustom org-files-db-directories nil
  "List of one or multiple directories to parse the org files in."
  :group 'org-files-db
  :type '(repeat directory))

(defcustom org-files-db-check-for-changes-interval 300
  "Interval in seconds to check if any file has been changed.
If any file has been modified, the database is updated."
  :group 'org-files-db
  :type 'number)

(defcustom org-files-db-db-path (locate-user-emacs-file "org-files-db.sqlite")
  "The path where the database is stored. Use .db or .sqlite as extension."
  :group 'org-files-db
  :type 'string)

(defcustom org-files-db-files-exclude-regexp nil
  "Org files matching this regular expression are excluded."
  :group 'org-files-db
  :type 'string)

;; * Initialize

;; * DB

;; ** DB Core

;; ** DB Build

;; ** DB Insert & Update

;; ** DB Queries

;; * Parse Files

;; Uses ugrep (https://github.com/Genivia/ugrep) to parse the org files.



;; * File Management

;; * Footer

(provide 'org-files-db)

;;; org-files-db.el ends here
