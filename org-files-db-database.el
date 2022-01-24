;;; org-files-db-database.el --- Org-files-db Database -*- lexical-binding: t -*-

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

;; Variables and functions to interact with the database.

;;; Code:

;; * Requirements

(require 'emacsql)
(require 'emacsql-sqlite)
(require 'org-files-db-core)

;; * Variables

(defconst org-files-db--db-version 1
  "The version of the database.
Make sure to update this if the `org-files-db--db-schema' or the
`org-files-db--db-indices' are changed. If the database version changes it will
be rebuilt from scratch.")

(defvar org-files-db--db-connection nil
  "Process that runs the SQLite3 interative shell.")

(defconst org-files-db--db-process-name "org-files-db"
  "The name for the process. Is also used to name the buffer.")

(defvar org-files-db--db-sqlite-output nil
  "The output of the SQLite3 interactive shell.")

(defconst org-files-db--db-schema
  ;; https://www.sqlitetutorial.net/sqlite-create-table/
  '(
    ;; All the directories in which the org-files are parsed.
    ("CREATE TABLE IF NOT EXISTS directories (
directory text NOT NULL PRIMARY KEY,
-- Updated, mtime and size are used to make sure the directory is not
-- dirty. Updated and mtime stored as seconds since the epoch.
updated integer NOT NULL, mtime integer NOT NULL, size integer NOT NULL);")
    ;; Metadata of the org files in those directories.
    ("CREATE TABLE IF NOT EXISTS files (
filename text NOT NULL PRIMARY KEY, directory text NOT NULL,
-- Updated, mtime and size are used to make sure the file is not dirty.
-- Updated and mtime stored as seconds since the epoch.
updated integer NOT NULL, mtime integer NOT NULL, size integer NOT NULL,
title text,
FOREIGN KEY (directory) REFERENCES directories (directory) ON DELETE CASCADE);")
    ;; Metadata of the headings in the org files.
    ("CREATE TABLE IF NOT EXISTS headings (
id integer NOT NULL PRIMARY KEY, file text NOT NULL,
-- The level of the heading. An artificial level 0 heading
-- is added to store file level properties and metadata.
level integer NOT NULL, position integer NOT NULL,
-- Store the full line text of the heading including stars.
full_text text,
-- Components of the heading.
priority text NOT NULL, todo_keyword text, title text, statistic_cookies text,
-- Store planning info as float to be able to store date and time.
scheduled real, deadline real, closed real,
-- Self reference to the parent id.
parent_id integer,
UNIQUE(file, position),
FOREIGN KEY (file) REFERENCES files (file) ON DELETE CASCADE,
FOREIGN KEY (parent_id) REFERENCES headings (id) ON DELETE CASCADE);")
    ;; Tags per heading.
    ("CREATE TABLE IF NOT EXISTS tags (
heading_id integer NOT NULL, tag text NOT NULL,
PRIMARY KEY (heading_id, tag),
FOREIGN KEY (heading_id) REFERENCES headings (id) ON DELETE CASCADE);")
    ;; Properties per heading.
    ("CREATE TABLE IF NOT EXISTS properties (
heading_id integer NOT NULL, property text NOT NULL, value text,
PRIMARY KEY (heading_id, property),
FOREIGN KEY (heading_id) REFERENCES headings (id) ON DELETE CASCADE);")
    ;; Links in the files.
    ("CREATE TABLE IF NOT EXISTS links (
file text NOT NULL, position integer NOT NULL,
full_link text NOT NULL, type text, link text NOT NULL, description text,
PRIMARY KEY (file, position),
FOREIGN KEY (file) REFERENCES files (file) one DELETE CASCADE);")
    ;; The virtual table for full text search.
    ("CREATE VIRUTAL TABLE IF NOT EXISTS files_fts
USING fts5 (file, content);"))
  "List of SQL statements to create the tables.")

(defconst org-files-db--db-indices
  ;; https://www.sqlitetutorial.net/sqlite-index/
  '(("CREATE INDEX headings_title_id ON headings(title);"))
  "List of SQL statements to create the indices.
No indices are create if this is set to nil")

;; * Core

(defun org-file-db--db-execute (db sql &optional no-transaction silent)
  "Execute an SQL statement in the SQLite3 shell for the DB.
SQL can be a string or a list of string. If the SQL is a list of statements a
transaction is used unless NO-TRANSACTION is non-nil. If SILENT is non-nil no
output is shown."
  (if silent
      (set-process-filter db t)
    (set-process-filter db #'org-files-db--db-message-output))
  (if (stringp sql)
      (process-send-string
       db (format "%s\n" (org-file-db--db-fix-sql-statement sql)))
    (unless no-transaction
      (process-send-string db "BEGIN TRANSACTION;"))
    (dolist (statement sql)
      (process-send-string
       db (format "%s\n" (org-file-db--db-fix-sql-statement statement))))
    (unless no-transaction
      (process-send-string db "COMMIT;"))))

(defun org-file-db--db-fix-sql-statement (sql)
  "This just verifies if there is a semicolon at the end of the SQL.
If not the semicolon is added."
  (if (string-suffix-p ";" sql)
      sql
    (format "%s;" sql)))

(defun org-file-db--db-execute-commmand (db command)
  "Execute an SQLite dot COMMAND in the SQLite3 shell for the DB.
Those commands start with a dot and are not terminated with a semicolon."
  (unless (and (string-prefix-p "." command)
               (not (string-suffix-p ";" command)))
    (user-error "The command '%s' is not valid" command))
  (process-send-string db (format "%s\n" command)))

(defun org-file-db--db-get-output (db sql &optional object-type raw)
  "Execute SQL string in the SQLite3 shell for DB and return result.
The DB is configured to output JSON string. The JSON returned is parsed into a
lips object used the OBJECT-TYPE which defaults to `hash-table'. Other
object-types are `alist' or `plist'. If RAW is non-nil the raw JSON string is
returned."
  (set-process-filter db #'org-files-db--db-capture-output)
  (unless (accept-process-output
           (process-send-string
            db (format "%s\n" (org-file-db--db-fix-sql-statement sql)))
           org-files-db-db-timeout)
    (error "Timeout reached before output was received."))
  (if raw
      org-files-db--db-sqlite-output
    (let ((type (or object-type 'hash-table)))
      (json-parse-string org-files-db--db-sqlite-output :object-type type))))

(defun org-files-db--db-message-output (_process output)
  "Just show a message with the output."
  (message "Org-files-db SQLite output: %s" output))

(defun org-files-db--db-capture-output (_process output)
  "Store the output in a variable."
  (setq org-files-db--db-sqlite-output output))

;; * Create

;; SQLite Dataypes: integer, real, text, blob

(defun org-files-db--db-create (path)
  "Create the database at PATH. If it already exists it is overwritten.
Use the `org-files-db--db-schema' to create the tables and also create the
INDICES taken from `org-files-db--db-indices'. If the database exists the
existing tables will be dropped. The user_version is set to
`org-files-db--db-version'. This sets the `org-files-db--db-connection'.
Returns the connection (the process object)."
  ;; Create the file at path. It will be overwritten if it already exists.
  (org-files-db--db-create-db-file path)
  (let* ((process (org-files-db--db-connect path org-files-db--db-process-name
                                            org-files-db-db-timeout t))
         (schema org-files-db--db-schema)
         (indices org-files-db--db-indices)
         (version org-files-db--db-version))
    ;; No output unless a function explicitly wants it.
    (set-process-filter process t)
    ;; Store the connection.
    (setq org-files-db--db-connection process)
    ;; Create the tables and the indices.
    ;; (org-files-db--db-create-tables process schema)
    ;; (org-files-db--db-create-indices process indices)
    ;; Set the user version.
    (org-files-db--db-set-user-version process version)
    process))

(defun org-files-db--db-exists-p (path)
  "Check if the database file with PATH exists."
  (file-exists-p path))

(defun org-files-db--db-create-db-file (path)
  "Create the database file at PATH.
Will overwrite existing files and creates parent directories if needed."
  (make-empty-file path t))

(defun org-files-db--db-set-user-version (db version)
  "Set the user_version to VERSION for the DB."
  (org-file-db--db-execute
   db (format "PRAGMA user_version = %s;" version)))

(defun org-files-db--db-get-user-version (db)
  "Get the user_version of the open DB connection."
  (plist-get
   (seq-first (org-file-db--db-get-output db "PRAGMA user_version;" 'plist))
   :user_version))

;; * Connection (process)

(defun org-files-db--db-connect (path name timeout &optional force)
  "Start an interactive SQLite3 shell against the existing db at PATH.
Uses the NAME for the process and its buffer. If FORCE is non-nil an already
existing process will be killed and a new process is started. Foreign keys and
sets output mode to json are enabled. Returns the process object.
Set the TIMEOUT for the database as the default is 0 in the interactive shell."
  (let ((running-process (get-process name))
        (process-buffer-name (format " *%s* " name)))
    (when (and (process-live-p running-process) force)
      (org-files-db--db-disconnect running-process))
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
        ;; Call the sentinel when the process state changes.
        (set-process-sentinel process
                              #'org-files-db--db-handle-process-status-change)
        ;; Enable foreign keys and set output mode.
        (org-file-db--db-execute process "PRAGMA foreign_keys = ON;")
        (org-file-db--db-execute
         process (format "PRAGMA busy_timeout=%s" (* 1000  timeout)))
        (org-file-db--db-execute-commmand process ".mode json")
        process))))

(defun org-files-db--db-disconnect (db)
  "Ends the Close the DB connection."
  (when (process-live-p db)
    (process-send-eof db))
  (when (buffer-live-p (process-buffer db))
    (kill-buffer (process-buffer db))))

(defun org-files-db--db-handle-process-status-change (process event)
  "Handle changes of the `process-status'.
This function is set with `set-process-sentinel'. On status changes the db is disconnected. If this"
  (message "Org-files-db: %s had the event '%s'." process event)
  (org-files-db--db-disconnect process))

;; * Create

(defun org-files-db--db-create-tables (db schema)
  "Create the tables in the connected DB with the SCHEMA provided.
Check `org-files-db--db-schema' on how to define a schema."
  (emacsql-with-transaction db
                            (pcase-dolist (`(,table ,schemata) schema)
                              (emacsql db [:create-table :if-not-exists $i1 $S2] table schemata))))

(defun org-files-db--db-create-indices (db indices)
  "Create the INDICES in the connected DB.
Check `org-files-db--db-indices' on how to define the indices."
  (when indices
    (emacsql-with-transaction db
                              (pcase-dolist (`(,index-name ,table ,columns) indices)
                                (emacsql db [:create-index $i1 :on $i2 $S3] index-name table columns)))))

;; * Insert

;; Insert directory

(defun org-files-db--db-insert-directory (db dir updated mtime size)
  "Insert directory DIR into the directory table in the connected DB.
Also store the current time UPDATED, the MTIME (modification time) and the SIZE
in bytes. Times are stored as seconds since the epoch."
  (emacsql db [:insert :into directories :values $v1]
           (vector dir updated mtime size)))

;; Insert file

(defun org-files-db--db-insert-file (db file dir updated mtime size title)
  "Insert FILE into the files table in the connected DB.
Also store the current time UPDATED, the MTIME (modification time) and the SIZE
in bytes. Times are stored as seconds since the epoch. If there is a title in
the org file it is stored as well."
  (emacsql db [:insert :into files :values $v1]
           (vector file dir updated mtime size title)))

;; Insert heading

(defun org-files-db--db-insert-heading (db file level pos title parent-id
                                           &optional prio todo  cookies
                                           scheduled deadline closed)
  "Insert a heading into the headings table in the connected DB.
Needs the FILE, LEVEL, POS (position), TITLE and PARENT-ID. The other arguments
are optional as not available for all headings: PRIO, TODO keyword, statistic
COOKIES and planning info (SCHEDULED, DEADLINE, CLOSED)."
  (emacsql db [:insert :into headings :values $v1]
           (vector file level pos prio todo title cookies scheduled deadline
                   closed parent-id)))

;; Insert tag

(defun org-files-db--db-insert-tag (db heading-id tag)
  "Insert TAG of a heading into the tags table in the DB.
References to the heading by the id HEADING-ID stored in the db."
  (emacsql db [:insert :into tags :values $v1]
           (vector heading-id tag)))

;; Insert property

(defun org-files-db--db-insert-property (db heading-id property value)
  "Insert PROPERTY and VALUE of a heading into the properties table in the DB.
References to the heading by the id HEADING-ID stored in the db."
  (emacsql db [:insert :into properties :values $v1]
           (vector heading-id property value)))

;; Insert link

(defun org-files-db--db-insert-link (db file pos full-link type link description)
  "Insert a link in a FILE into the DB.
Stores the references to the FILE, POS (position), full text of
the link (FULL-LINK), TYPE of the link, LINK and DESCRIPTION."
  (emacsql db [:insert :into links :values $v1]
           (vector file pos full-link type link description)))

;; * Update

;; Update directory

;; * Delete

;; Update directory

;; * Drop

;; Drop directory
;; Drop file

;; * Select

;; * FTS Full Text Search

;; ** FTS Create

(defun org-files-db--db-fts-create-tables (db)
  "Create the virtual table for fts in the connected DB.
Uses the default tokenizer unicode61 as porter only works for english."
  (emacsql-with-transaction db
                            (emacsql db [:create-virtual-table :if-not-exists files_fts
                                                               :using :fts5 [(filename content)]])))

;; ** FTS Insert

(defun org-files-db--db-fts-insert-file (db file)
  "Read content FILE into the files_fts table in the connected DB.
The readfile function provided by SQLite is used."
  ;; TODO This can't be done. Opened an issue:
  ;; https://github.com/skeeto/emacsql/issues/88
  ;; Probably better to just use a simple
  (emacsql db [:insert :into files_fts :values ([filename readfile([test])])]
           file ))

;; (defun org-files-db--db-fts-insert-file-shell (path file)
;;   "Read content FILE into the files_fts table in the connected DB.
;; The readfile function provided by SQLite is used."
;;   ;; TODO This can't be done. Opened an issue:
;;   ;; https://github.com/skeeto/emacsql/issues/88
;;   ;; Probably better to just use a process to run a shell command or script.
;;   ;; This might be better and probably faster for evertying here expect queries.
;;   ;; Ok, emacsql accepts pure strings as well as SQL, but looks like the version
;;   ;; used has not loaded the extension readfile. So just do it with a process
;;   ;; async.
;;   (emacsql db [:insert :into files_fts :values ([filename readfile([test])])]
;;            file ))

;; (start-process-shell-command "sleep" "*sleep*" "sleep 5 && echo wake")
;; (delete-process)

;; ** FTS Delete

;; * Footer

(provide 'org-files-db-database)

;;; org-files-db-database.el ends here
