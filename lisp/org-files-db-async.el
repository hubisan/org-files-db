;;; org-files-db-async.el --- Async wrapper for org-files-db  -*- lexical-binding: t; -*-

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

(require 'async)
(require 'org-files-db-watch)

;;; Constants & Variables

;;; Utility

;;; Watch

;; TODO vom ChatGPT erster Vorschlag
(defgroup org-files-db-watch-async nil
  "Asynchronous execution for org-files-db-watch."
  :group 'org-files-db-watch)

(defcustom org-files-db-watch-async-interval 300
  "Seconds between asynchronous scan executions."
  :type 'integer
  :group 'org-files-db-watch-async)

(defvar org-files-db-watch-async--timer nil
  "Internal timer for triggering async watch scans.")

(defun org-files-db-watch-async--run-once ()
  "Run one asynchronous watch scan in a separate process."
  (message "[org-files-db-watch-async] Starting async scan...")
  (async-start
   ;; ---- child process ----
   `(lambda ()
      (require 'org)
      (require 'org-files-db-watch)
      ;; Important: set lexical-binding in child too
      (let ((org-files-db-watch-directories ',org-files-db-watch-directories))
        (org-files-db-watch--scan-and-compare)))
   ;; ---- callback in parent ----
   (lambda (diff)
     (when diff
       (cl-destructuring-bind (&key added changed deleted) diff
         (message "[org-files-db-watch-async] Done. Added: %d, Changed: %d, Deleted: %d"
                  (length added) (length changed) (length deleted)))
       ;; Optionally trigger reindex or DB updates here:
       ;; (org-files-db-indexer-process-diff diff)
       ))))

(defun org-files-db-watch-async-start ()
  "Start periodic asynchronous scans."
  (interactive)
  (org-files-db-watch-async-stop)
  (setq org-files-db-watch-async--timer
        (run-with-timer 0 org-files-db-watch-async-interval
                        #'org-files-db-watch-async--run-once))
  (message "[org-files-db-watch-async] Started async watcher every %ds"
           org-files-db-watch-async-interval))

(defun org-files-db-watch-async-stop ()
  "Stop periodic asynchronous scans."
  (interactive)
  (when (timerp org-files-db-watch-async--timer)
    (cancel-timer org-files-db-watch-async--timer)
    (setq org-files-db-watch-async--timer nil)
    (message "[org-files-db-watch-async] Stopped async watcher.")))

;;; Update

;; org-files-db-async--apply-changes
;; org-files-db-async--reindex-file
;; org-files-db-async--delete-file

;;; Provide

(provide 'org-files-db-async)

;;; org-files-db-async.el ends here
