;;; org-files-db-core.el --- Customization, common vars and functions  -*- lexical-binding: t -*-

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

;; Customization: variables and faces.

;;; Code:

;; * Requirements

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

(defcustom org-files-db-db-full-text-search-enabled-p t
  "If non-nil full text search is enabled.
This will store the content of all org files from the directories in a seconds
database. Takes additional space."
  :group 'org-files-db
  :type 'boolean)

(defcustom org-files-db-db-timeout 30
  "Maximum number of seconds to wait before bailing out on a SQL command."
  :group 'org-files-db
  :type 'integer)

;; * Variables

;; * Functions

;; * Footer

(provide 'org-files-db-core)

;;; org-files-db-core.el ends here
