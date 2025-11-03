;;; org-files-db-core.el --- Core configuration for org-files-db -*- lexical-binding: t; -*-

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

;; Core configuration and shared constants for org-files-db.
;; This file defines all customization options, global variables,
;; and basic helper functions. It does not directly access the database.

;;; Code:

;;; Customization

(defgroup org-files-db nil
  "Store headings and links from Org files in a SQLite database."
  :group 'org
  :link '(url-link :tag "GitHub" "https://github.com/hubisan/org-files-db")
  :prefix "org-files-db-")

(defcustom org-files-db-source-paths nil
  "List of directories and/or files to be scanned for Org files.
Each element can be either a directory or a specific Org file.
All matching Org files will be parsed and stored in the SQLite database."
  :group 'org-files-db
  :type '(repeat (file :tag "Directory or Org File")))

(defcustom org-files-db-files-exclude-regexps nil
  "List of regular expressions to exclude matching file names."
  :group 'org-files-db
  :type '(repeat string))

(defcustom org-files-db-exclude-file-regexps nil
  "List of regular expressions to exclude certain files from parsing.
Any file whose name matches one of these regular expressions will be skipped."
  :group 'org-files-db
  :type '(repeat (regexp :tag "Exclude File Pattern")))

(defcustom org-files-db-database-file
  (locate-user-emacs-file "org-files.db")
  "Path to the SQLite database file used by `org-files-db'.
It is recommended to use a `.db` or `.sqlite` extension for the file."
  :group 'org-files-db
  :type 'file)

(defcustom org-files-db-check-interval 300
  "Interval (in seconds) for checking changes in Org files.
If any files have been modified since the last check, the database will
be updated accordingly.  Set this to `nil` to disable automatic checks."
  :group 'org-files-db
  :type 'number)

;;; Global Constants & Variables

(defconst org-files-db--version "0.1.0"
  "The `org-files-db' version.")

(defconst org-files-db--install-directory
  (file-name-directory
   (or load-file-name buffer-file-name (locate-library "org-files-db") nil))
  "The directory where the `org-files-db' package is installed.")

;;; Helper Functions

(defun org-files-db-version ()
  "Show the `org-files-db' version."
  (interactive)
  (if (called-interactively-p 'interactive)
      (message "Org-files-db version %s" org-files-db--version)
    org-files-db--version))

(provide 'org-files-db-core)
;;; org-files-db-core.el ends here
