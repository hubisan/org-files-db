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

(defun org-files-db--sqlite-process-start (path name timeout &optional force)
  "Start a process that runs an interactive SQLite3 shell.
Uses the NAME for the process and its buffer. If FORCE is non-nil an already
existing process will be killed and a new process is started. Foreign keys and
sets output mode to json are enabled. Returns the process object.
Set the TIMEOUT for the database as the default is 0 in the interactive shell."
  (let ((running-process (get-process name))
        (process-buffer-name (format " *%s* " name)))
    (when (and (process-live-p running-process) force)
      (org-files-db--db-disconnect running-process)
      (delete-process running-process))
    (if (process-live-p running-process)
        running-process
      ;; Start a new process.
      (when (buffer-live-p (get-buffer process-buffer-name))
        (kill-buffer process-buffer-name))
      (let* ((process-connection-type nil)  ; use a pipe
             (coding-system-for-write 'utf-8-auto)
             (coding-system-for-read 'utf-8-auto)
             (buffer (generate-new-buffer process-buffer-name))
             (process (start-process name buffer "sqlite3"
                                     (expand-file-name path))))
        ;; Don't show any output unless it is needed.
        (set-process-filter process t)
        ;; Call the sentinel when the process state changes.
        (set-process-sentinel process
                              #'org-files-db--db-handle-process-status-change)
        ;; Enable foreign keys and set output mode.
        (org-files-db--db-execute process "PRAGMA foreign_keys = ON;" t t)
        (org-files-db--db-execute
         process (format "PRAGMA busy_timeout=%s" (* 1000  timeout)) t t)
        (org-files-db--db-execute-commmand process ".mode json")
        process))))

(defun org-files-db--sqlite-process-delete (&optional process)
  "Ends the Close the DB connection."
  (set-process-sentinel process nil)
  (when (process-live-p process)
    (process-send-eof process))
  (when (buffer-live-p (process-buffer process))
    (kill-buffer (process-buffer process)))
  (delete-process process))

(defun org-files--sqlite-process-get (&optional process)
  "Return the PROCESS.
If the PROCESS is nil the default stored in `org-files-db--sqlite-process' is
returned. This also makes a simple check to see if the process is valid."
  (let ((process (or process org-files-db--sqlite-process)))
    (cond ((not (process-live-p process))
           (user-error "Process is not live anymore"))
          ((not (string-equal (nth 1 (process-command process)) "sqlite3"))
           (user-error "Process %s is not running a SQLite3 shell" process ))
          (t process))))

(defun org-files-db--sqlite-process-set-default (process)
  "Set the process that is used by the default.
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

(defun org-files-db--sqlite-process-filter-message-output (process output)
  "Just show a message with the OUTPUT."
  (message "SQLite '%s': '%s'" process output))

(defun org-files-db--sqlite-process-filter-capture-output (_process output)
  "Store the OUTPUT in a variable."
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
MODE: If MODE is nil no output is shown and the statement is run asynchronously. If
MODE is any of the following symbols the mode is set with .mode before executing
the statement and the output is returned: ascii, box, csv, column, html, json,
line, list, markdown, quote, table, tabs, tcl. You can check the supported modes
of your SQLite version inside the shell with '.help mode'. If mode is non-nil it
is waiting for the process to finish to be able to return the output else it is
not waiting.
TIMEOUT: Set the TIMEOUT in seconds. IF TIMEOUT is nil the timeout stored in
`org-files-db--sqlite-timeout' is used.
PROCESS: If PROCESS is nil the process stored in `org-files-db--sqlite-process'
is used."
  (let ((process (org-files--sqlite-process-get process))
        (timeout (or timeout org-files-db--sqlite-timeout)))
    ;; Silence it and set the timeout.
    (set-process-filter process t)
    (process-send-string
               process (format "PRAGMA busy_timeout=%s" (* 1000 timeout)))
    (if mode
        (unwind-protect
            (progn
              ;; Set the mode.
              (org-files-db--sqlite-execute-commmand (concat ".mode " mode))
              ;; Set the filter to send the output to.
              (set-process-filter process #'org-files-db--sqlite-capture-output)
              ;; Execute the statement and wait for the output.
              (unless
                  (accept-process-output
                   (process-send-string process (format "%s\n" sql)) timeout)
                (error "Timeout reached before output was received"))
              org-files-db--sqlite-output)
          (set-process-filter process t))
      (process-send-string process (format "%s\n" sql)))))

(defun org-files-db--sqlite-execute-from-file (db path &optional silent)
  "Execute an SQL statement stored in a file in the SQLite3 shell run in DB.
If SILENT is non-nil no output is shown. PATH has to be the absolute path of
the file."
  (if silent
      (set-process-filter db t)
    (set-process-filter db #'org-files-db--db-message-output))
  (unwind-protect
      (process-send-string db (format ".read %s\n" (expand-file-name path)))
    (set-process-filter db t)))

(defun org-files-db--sqlite-execute-commmand (db command)
  "Execute an SQLite dot COMMAND in the SQLite3 shell with DB."
  (process-send-string db (format "%s\n" command)))

;; * Footer

(provide 'org-files-db-sqlite)

;;; org-files-db-sqlite.el ends here
