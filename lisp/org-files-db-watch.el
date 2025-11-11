;;; org-files-db-watch.el --- Periodic Org file change detection -*- lexical-binding: t; -*-

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

;; This module periodically scans directories containing Org files,
;; stores modification times in a hash table, and compares them to
;; the last known state (from the database).  If changes are detected,
;; appropriate update actions (like reindexing) can be triggered.

;;; Code:

;;; Requirements

(require 'org)
(require 'org-files-db-core)
(require 'org-files-db-database)
(require 'org-files-db-indexer)

;;; Constants & Variables

(defvar org-files-db-watch--timer nil
  "Internal timer for periodic file checks.")

;;; Utility

;;; Scanning

;; TODO Irgendwie noch die Dateien ausschliessen, welche mit regexp ignoriert
;; werden.
(defun org-files-db-watch--get-org-files-recursively (path &optional result)
  "Recursively collect all Org files from PATH and record their modification times.
Return a hash table mapping absolute file names to their modification times,
represented as floating-point Unix timestamps (seconds since the epoch, possibly
with fractional seconds).
Only regular files with the \".org\" extension are included.  Directories are
traversed recursively.
If RESULT is non-nil, use it as the hash table accumulator and add results into it."
  (let ((result (or result (make-hash-table :test 'equal)))
        (entries (condition-case nil
                     (directory-files-and-attributes path t "\\`[^.]")
                   (file-error nil))))
    (dolist (entry entries)
      (let* ((file (car entry))
             (attrs (cdr entry))
             (type  (file-attribute-type attrs)))
        (cond
         ;; Regular .org file (type == nil)
         ((and (null type)
               (string-suffix-p ".org" file))
          (let ((mtime (float-time (file-attribute-modification-time attrs))))
            (puthash file mtime result)))

         ;; Directory (type == t)
         ((and (eq type t)
               (file-readable-p file))
          (org-files-db-watch--get-org-files-recursively file result)))))
    result))

;;; Compare

;; TODO
;; Compare and somehot return added, changed and deleted.
;; On first scan I need to get the hash with the above function
;; Then make a hash from the database files.
;; On the seconds scan I can compare new and old hash.
;; Added and deleted are easy.
;; But if the mtime changed I have to compare the md5 from the db with the
;; current. I don't plan to store this in the hash, as I would scan loads of
;; files even if not needed.
(defun org-files-db-watch--compare (old new)
  ""
  ;; This is the funciton from chatgpt, needs a lot of changes i think.
  (let ((added '())
        (changed '())
        (deleted '()))
    ;; Find added and potentially changed files
    (maphash
     (lambda (file new-mtime)
       (let* ((old-entry (gethash file old))
              (old-mtime (plist-get old-entry :mtime))
              (old-md5   (plist-get old-entry :md5)))
         (cond
          ;; brand-new file
          ((not old-entry)
           (push file added))
          ;; mtime differs — need to verify via MD5
          ((and old-mtime (not (equal old-mtime new-mtime)))
           (let ((new-md5 (ignore-errors (md5 (with-temp-buffer
                                                (insert-file-contents-literally file)
                                                (current-buffer))))))
             (unless (equal new-md5 old-md5)
               (push file changed)))))))
     new)

    ;; Find deleted files
    (maphash
     (lambda (file _)
       (unless (gethash file new)
         (push file deleted)))
     old)

    (list :added added :changed changed :deleted deleted)))

;;; Periodic check logic

;; TODO from chattpt, but this needs a lot of changes
(defun org-files-db-watch--scan-and-compare ()
  "Perform one scan cycle and detect file changes."
  (let* ((current (make-hash-table :test 'equal))
         ;; gather all directories into one combined hash
         (_ (dolist (dir org-files-db-watch-directories)
              (when (file-directory-p dir)
                (org-files-db-watch--get-org-files-recursively dir current))))
         (previous (org-files-db-database-get-all-mtimes)) ;; hypothetical DB accessor
         (diff (org-files-db-watch--diff-hashes previous current)))
    (cl-destructuring-bind (&key added changed deleted) diff
      (when (or added changed deleted)
        (message "[org-files-db-watch] Added: %d, Changed: %d, Deleted: %d"
                 (length added) (length changed) (length deleted)))
      ;; TODO: trigger reindex/update actions:
      ;; (dolist (f added)   (org-files-db-indexer-add f))
      ;; (dolist (f changed) (org-files-db-indexer-update f))
      ;; (dolist (f deleted) (org-files-db-database-remove f))
      )))

;;; Timer management

;; TODO Gehört wohl eher nach Async
(defun org-files-db-watch-start ()
  "Start periodic scanning for Org file changes."
  (interactive)
  (org-files-db-watch-stop)
  (setq org-files-db-watch--timer
        (run-with-timer 0 org-files-db-watch-interval
                        #'org-files-db-watch--scan-and-compare))
  (message "[org-files-db-watch] Started periodic check every %ds."
           org-files-db-watch-interval))

(defun org-files-db-watch-stop ()
  "Stop periodic file scanning."
  (interactive)
  (when (timerp org-files-db-watch--timer)
    (cancel-timer org-files-db-watch--timer)
    (setq org-files-db-watch--timer nil)
    (message "[org-files-db-watch] Stopped periodic watcher.")))

;;; Provide

(provide 'org-files-db-watch)

;;; org-files-db-watch.el ends here
