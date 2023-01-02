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

;; This file contains customizations, faces, constants, global variables and
;; auxiliary functions.

;;; Code:

;;;; * Requirements

;;;; * Customization

(defgroup org-files-db nil
  "Store headings and links from org files in a SQLite database."
  :group 'org
  :link '(url-link :tag "Github" "https://github.com/hubisan/org-files-db")
  :prefix "org-files-db-")

(defcustom org-files-db-directories nil
  "List of one or multiple directories to parse the Org files in."
  :group 'org-files-db
  :type '(repeat directory))

(defcustom org-files-db-files-exclude-regexps nil
  "List of regular expressions to exclude matching file names."
  :group 'org-files-db
  :type '(repeat string))

(defcustom org-files-db-file (locate-user-emacs-file "org-files.db")
  "Filename used for the database. Use .db or .sqlite as extension."
  :group 'org-files-db
  :type 'string)

(defcustom org-files-db-check-for-changes-interval 300
  "Interval in seconds to check if any file has been changed.
If any file has changed, the database is updated accordingly."
  :group 'org-files-db
  :type 'number)

(defcustom org-files-db-full-text-search-enabled t
  "If non-nil full text search is enabled.
This will store the content of the org files from the directories in a second
database to be able to use full-text-search provided by SQLite."
  :group 'org-files-db
  :type 'boolean)

(defcustom org-files-db-full-text-search-file
  (locate-user-emacs-file "org-files-fts.db")
  "Filename used for the full text search database.
Use .db or .sqlite as extension."
  :group 'org-files-db
  :type 'string)

;;;; * Faces

;;;; * Constants

(defconst org-files-db--version "v0.1.0"
  "The `org-files-db' version.")

(defconst org-files-db--load-dir (file-name-directory
                                  (or load-file-name buffer-file-name))
  "The directory where this package is stored.")

;;;; * Global Variables


;;;; * Auxiliary Functions

(defun org-files-db-version ()
  "Return the `org-files-db' version."
  (interactive)
  (when (called-interactively-p 'interactive)
    (message "%s" org-files-db--version))
  org-files-db--version)

;;;; * Footer

(provide 'org-files-db-core)

;;; org-files-db-core.el ends here
