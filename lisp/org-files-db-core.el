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

;;; Requirements

;;; Customization

(defgroup org-files-db nil
  "Store headings and links from Org files in a SQLite database."
  :group 'org
  :link '(url-link :tag "GitHub" "https://github.com/hubisan/org-files-db")
  :prefix "org-files-db-")

(defcustom org-files-db-debug nil
  "If non-nil, print debug log messages as well."
  :group 'org-files-db
  :type 'boolean)

(defcustom org-files-db-source-paths nil
  "List of Org file sources (directories or files) to be scanned and indexed.

Each element defines a source location and may be one of the following:
- A string: a directory or filename. Directories are scanned recursively.
  Directories may or may not include a \"/\" at the end.
- A cons cell (DIRECTORY . nil): a directory that should be scanned,
  but not recursively.

Paths have to be absolute (e.g., \"/home/user/org/\"). The home directory can
be abbreviated with \"~\" (e.g., \"~/org/\"). If it is a file the extension
\".org\" must be included.

Example:
  '(\"~/org/projects\"
    ;; Disable recursive scanning for this directory:
    (\"~/org/archive\" . nil)
    ;; Include specific files explicitly:
    \"~/notes/personal.org\"
    \"/home/user/memento/ideas.org\")"
  :group 'org-files-db
  :type '(repeat
          (choice
           (file :tag "Directory or Org File (recursive)")
           (cons :tag "Directory (non-recursive)"
                 (file :tag "Path")
                 (const :tag "Recursive?" nil)))))

(defcustom org-files-db-exclude-files-regexps nil
  "List of regular expressions to exclude Org files from scanning and parsing.

Each pattern is matched against the absolute path of a file using
`string-match-p', where the home directory is represented by \"~\".

Examples:
  - \"~/memento/secret/\"
    → excludes all files inside the ~/memento/secret directory
  - \"private-note\"
    → excludes any Org file whose name includes 'private-note'
  - \"private-note.*\\.org$\"
    → excludes all files whose name ends with 'private-note.org'"
  :group 'org-files-db
  :type '(repeat (regexp :tag "Exclude File Pattern")))


(defcustom org-files-db-database-file
  (locate-user-emacs-file "org-files.sqlite")
  "Absolute path to the SQLite database file used by `org-files-db'.

By default, the database file is stored in the user's Emacs directory (see
`locate-user-emacs-file') as \"org-files.sqlite\". You can change this to
place the database elsewhere.

Examples:
  - \"~/org/org-files.sqlite\"
  - \"/home/user/org/org-files.sqlite\""
  :group 'org-files-db
  :type 'file)

(defcustom org-files-db-check-interval 300
  "Interval (in seconds) for checking changes in Org files.
If any files have been modified since the last check, the database will
be updated accordingly.  Set this to `nil' to disable automatic checks."
  :group 'org-files-db
  :type 'number)

(defcustom org-files-db-parse-options
  '(:include-tags t
    :include-keywords t
    :include-properties t
    :include-links t
    :include-timestamps t)
  "Default options controlling which elements are parsed from Org files.

This plist defines which parts of an Org file are extracted when parsing:
- `:include-tags'
  Include heading tags (e.g., :work:urgent:).
- `:include-keywords'
  Include file-level #+KEYWORD lines like #+TITLE or #+CATEGORY.
- `:include-properties'
  Include property drawers and key–value pairs under headlines.
- `:include-links'
  Include Org links such as [[file:...]] or [[https://...]].
- `:include-timestamps'
  Include scheduling and deadline timestamps (SCHEDULED, DEADLINE, CLOSED)."
  :group 'org-files-db
  :type '(plist
          :key-type (choice (const :include-tags)
                     (const :include-keywords)
                     (const :include-properties)
                     (const :include-links)
                     (const :include-timestamps))
          :value-type boolean))


;;; Constants & Variables

(defconst org-files-db--version "0.1.0"
  "The `org-files-db' version.")

(defconst org-files-db--install-directory
  (file-name-directory
   (or load-file-name buffer-file-name (locate-library "org-files-db") nil))
  "The directory where the `org-files-db' package is installed.")

;;; Utility

(defun org-files-db-version ()
  "Show the `org-files-db' version."
  (interactive)
  (if (called-interactively-p 'interactive)
      (message "Org-files-db version %s" org-files-db--version)
    org-files-db--version))

(defun org-files-db--get-directories (path)
  "Return a list of all readable subdirectories under PATH.
Hidden directories (starting with a dot) are skipped."
  (let (result (entries (directory-files path t "\\`[^.]")))
    (dolist (entry entries result)
      (when (and (file-directory-p entry)
                 (file-readable-p entry))
        (push entry result)
        (setq result (append result (org-files-db--get-directories entry)))))
    result))

;;; Logging

(defun org-files-db--log (format-string &rest args)
  "Log a message. Accepts printf-style FORMAT-STRING and ARGS."
  (let* ((timestamp (format-time-string "%M:%S"))
         (msg (apply #'format format-string args))
         (line (format "[org-files-db %s] %s" timestamp msg)))
    (message line)))

(defun org-files-db--debug-log (format-string &rest args)
  "Print a debug log message if `org-files-db-debug' is non-nil.
Prepends [DEBUG] to the message text."
  (when org-files-db-debug
    (org-files-db--log (concat "[DEBUG] " format-string) args)))

;;; Provide

(provide 'org-files-db-core)

;;; org-files-db-core.el ends here
