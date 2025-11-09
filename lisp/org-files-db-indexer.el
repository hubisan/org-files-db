;;; org-files-db-indexer.el --- Indexing and synchronization of Org files -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'org)

(require 'org-files-db-core)
(require 'org-files-db-parse)
(require 'org-files-db-database)

;;; Constants & Variables

(defvar org-files-db-indexer--indexer-running nil
  "Non-nil if an indexing operation is currently in progress.")

;;; Utility

(defun org-files-db-indexer--running-p ()
  "Return non-nil if the indexer is currently running."
  org-files-db-indexer--indexer-running)

;;; Handle Changes

(cl-defun org-files-db-indexer-handle-changes (&key added changed deleted)
  "Handle file changes detected by `org-files-db-watch'.
ADDED, CHANGED, and DELETED are lists of absolute paths.

If an indexing operation is already running, abort immediately."
  (if (org-files-db-indexer--running-p)
      (org-files-db-core--log
       "[INDEXER] Ignored change event — indexer already running.")
    (setq org-files-db-indexer--index-running t)
    (unwind-protect
         (progn
           (when added
             (dolist (absolute-path added)
               (org-files-db-indexer--add-file absolute-path)))
           (when changed
             (dolist (absolute-path changed)
               (org-files-db-indexer--update-file absolute-path)))
           (when deleted
             (dolist (absolute-path deleted)
               (org-files-db-indexer--delete-file absolute-path)))
           (org-files-db-core--log
            "[INDEXER] Processed %d added, %d changed, %d deleted files."
            (length added) (length changed) (length deleted)))
      ;; Always reset running flag
      (setq org-files-db-indexer--index-running nil))))

;;; Add, Delete & Update Files

(defun org-files-db-indexer--add-file (absolute-path)
  "Add the file located at ABSOLUTE-PATH to the database."
  )

(defun org-files-db-indexer--update-file (absolute-path)
  "Update the file located at ABSOLUTE-PATH in the database."
  ;; call delete then update.
  )

(defun org-files-db-indexer--delete-file (absolute-path)
  "Remove file located at ABSOLUTE-PATH from the database."
  )

;;; Provide

(provide 'org-files-db-indexer)

;;; org-files-db-indexer.el ends here
