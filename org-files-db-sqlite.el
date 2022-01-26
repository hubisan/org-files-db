;;; org-files-db-sqlite.el --- Interface to interact with SQLite database -*- lexical-binding: t -*-

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

;; SQLite Interface to interact with the SQLite interactive shell run in a
;; subprocess.

;;; Code:

;; * Requirements

(require 'cl-lib)

;; * Variables

(defvar org-files-db--sqlite-process nil
  "Process that runs the SQLite3 interative shell.")

(defconst org-files-db--sqlite-process-name "org-files-db"
  "The name for the process. Is also used to name the buffer.")

(defvar org-files-db--sqlite-output nil
  "The output of the SQLite3 interactive shell.")

(defvar org-files-db--sqlite-timeout 30
  "Maximum number of seconds to wait before bailing out on a SQL command.")

;; * Process

(defun org-files-db--sqlite-process-start (path name &optional force)
  "Start a process that runs an interactive SQLite3 shell and return it.
It connects to the SQLite file with PATH. Uses the NAME for the process and its
buffer. If FORCE is non-nil a live process will be killed and a new process is
started. If FORCE is nil an already live process is returned without starting a
new one. Returns the process object."
  (let ((running-process (get-process name))
        (process-buffer-name (format " *%s* " name))
        (path (expand-file-name path)))
    (unless (file-exists-p path)
      (user-error "File at path '%s' doesn't exist" path))
    ;; If force is non-nil and the process is live then delete it and its
    ;; buffer.
    (when (and (process-live-p running-process) force)
      (org-files-db--sqlite-process-delete running-process))
    ;; Return the process if it is live.
    (if (process-live-p running-process)
        running-process
      ;; Start a new process.
      (when (buffer-live-p (get-buffer process-buffer-name))
        (kill-buffer process-buffer-name))
      (let* ((process-connection-type nil)  ; use a pipe
             (coding-system-for-write 'utf-8-auto)
             (coding-system-for-read 'utf-8-auto)
             (buffer (generate-new-buffer process-buffer-name))
             (process (start-process name buffer "sqlite3" path)))
        ;; Don't show any output unless it is an error.
        (set-process-filter
         process #'org-files-db--sqlite-process-filter-check-for-error)
        ;; Call the sentinel when the process state changes.
        (set-process-sentinel
         process #'org-files-db--sqlite-process-sentinel-handle-status-change)
        process))))

(defun org-files-db--sqlite-process-configure (foreign-keys &optional
                                                            timeout process)
  "Configure the SQLite.
Turn FOREIGN-KEYS on if non-nil. If TIMEOUT is a number set the TIMEOUT before
the connection stops waiting for locks to clear in milliseconds. In the SQLite
shell this is usally set to 0 so you might increase this to wait if needed. If
the PROCESS is nil the default stored in `org-files-db--sqlite-process' is
returned."
  (let ((process (org-files-db--sqlite-process-get process)))
        (when foreign-keys
          (org-files-db--sqlite-execute
           "PRAGMA foreign_keys = ON;" nil nil process))
        (when timeout
          (org-files-db--sqlite-execute
           (format "PRAGMA busy_timeout=%s" timeout) nil nil process))))

(defun org-files-db--sqlite-process-delete (process)
  "Deletes the PROCESS and its buffer."
  (set-process-sentinel process nil)
  (when (process-live-p process)
    (process-send-eof process))
  (when (buffer-live-p (process-buffer process))
    (kill-buffer (process-buffer process)))
  (delete-process process))

(defun org-files-db--sqlite-process-get (&optional process)
  "Return the PROCESS.
If the PROCESS is nil the default stored in `org-files-db--sqlite-process' is
returned. This also makes a simple check to see if the process is valid."
  (let ((process (or process org-files-db--sqlite-process)))
    (cond ((not (process-live-p process))
           (user-error "Process is not live anymore"))
          ((not (string-equal (nth 0 (process-command process)) "sqlite3"))
           (user-error "Process %s is not running a SQLite3 shell" process ))
          (t process))))

(defun org-files-db--sqlite-process-set-default (process)
  "Set the PROCESS that is used by the default.
The default process is stored in `org-files-db--sqlite-process'.
Consider using `org-files-db--sqlite-with' instead."
  (setq org-files-db--sqlite-process process))

(defmacro org-files-db--sqlite-with (process &rest body)
  "Execute the forms in BODY with the PROCESS temporarily current.
The PROCESS has to run a interactive SQLite shell."
  (declare (indent 1) (debug t))
  `(let ((org-files-db--sqlite-process ,process))
     ,@body))

;; ** Filter & Sentinel

(defun org-files-db--sqlite-process-filter-check-for-error (process output)
  "Checks if the OUTPUT of the PROCESS is an error message."
  (when (string-prefix-p "Error: " output)
    (user-error "SQLite Error (%s): %s" process (substring output 7))))

(defun org-files-db--sqlite-process-filter-message-output (process output)
  "Just show a message with the OUTPUT from PROCESS."
  (org-files-db--sqlite-process-filter-check-for-error process output)
  (message "SQLite '%s': '%s'" process output))

(defun org-files-db--sqlite-process-filter-capture-output (process output)
  "Store the OUTPUT in a variable."
  (org-files-db--sqlite-process-filter-check-for-error process output)
  (setq org-files-db--sqlite-output output))

(defun org-files-db--sqlite-process-sentinel-handle-status-change (process event)
  "Handle changes of the `process-status' of the PROCESS.
The function gets two arguments: the PROCESS and the EVENT, a string describing
the change. This function is set with `set-process-sentinel'. On status changes
the db is disconnected."
  (message "Org-files-db: %s had the event '%s'." process event)
  ;; If process is not live anymore disconnect it;
  (unless (process-live-p process)
    (org-files-db--sqlite-process-delete process)))

;; * Execute SQL

(defun org-files-db--sqlite-execute (sql &optional mode timeout process)
  "Execute an SQL statement in the SQLite3 interactive shell.
MODE: If MODE is nil no output is shown and the statement is run asynchronously.
If MODE is any of the following symbols the mode is set with .mode before
executing the statement and the output is returned: ascii, box, csv, column,
html, json, line, list, markdown, quote, table, tabs, tcl. You can check the
supported modes of your SQLite version inside the shell with '.help mode'. If
mode is non-nil it is waiting for the process to finish to be able to return the
output else it is not waiting.
TIMEOUT: Set the TIMEOUT in seconds. IF TIMEOUT is nil the timeout stored in
`org-files-db--sqlite-timeout' is used.
PROCESS: If PROCESS is nil the process stored in `org-files-db--sqlite-process'
is used."
  (let ((process (org-files-db--sqlite-process-get process))
        (timeout (or timeout org-files-db--sqlite-timeout)))
    (setq org-files-db--sqlite-output nil)
    ;; Silence it.
    (set-process-filter
     process #'org-files-db--sqlite-process-filter-check-for-error)
    (if mode
        (unwind-protect
            (progn
              ;; Set the mode.
              (process-send-string
               process (format ".mode %s\n" (symbol-name mode)))
              ;; Set the filter to send the output to.
              (set-process-filter
               process #'org-files-db--sqlite-process-filter-capture-output)
              ;; Execute the statement and wait for the output.
              (unless
                  (accept-process-output
                   (process-send-string process (format "%s\n" sql)) timeout)
                (user-error "Timeout reached before output was received.
It either takes too long or that sql statement produces no output"))
              org-files-db--sqlite-output)
          (set-process-filter
           process #'org-files-db--sqlite-process-filter-check-for-error))
      (process-send-string process (format "%s\n" sql)))))

(defun org-files-db--sqlite-execute-from-file (path &optional mode timeout
                                                   process)
  "Execute an SQL statement stored in a file in the SQLite3 interactive shell.
PATH: The PATH of the file has to be an absolute one.
Please see `org-files-db--sqlite-execute' for the meaning of MODE and TIMEOUT.
PROCESS: If PROCESS is nil the process stored in `org-files-db--sqlite-process'
is used."
  (let* ((path (expand-file-name path))
         (sql (format ".read %s\n" (expand-file-name path))))
    (if (file-exists-p path)
        (org-files-db--sqlite-execute sql mode timeout process)
      (user-error "Path '%s' doesn't exist" path))))

;; * Footer

(provide 'org-files-db-sqlite)

;;; org-files-db-sqlite.el ends here
