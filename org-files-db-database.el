;;; org-files-db-database.el --- Interaction with the DB -*- lexical-binding: t -*-

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

(require 'seq)

(require 'org-files-db-core)
(require 'org-files-db-sqlite)

;; * Variables

(defconst org-files-db--database-version 1
  "The version of the database.
If the database version changes an existing database with an older version will
be rebuilt from scratch.")

(defvar org-files-db--database-process nil
  "Process that runs the SQLite3 interative shell.")

(defconst org-files-db--database-process-name "org-files-db"
  "The name for the process. Is also used to name the buffer.")

(defvar org-files-db--database-process nil
  "Process that runs the SQLite3 interative shell.")

(defconst org-files-db--database-schema-path
  (expand-file-name "sql/db-schema.sql" org-files-db--load-dir)
  "Path where the schema to create the database is stored.
This is relative to the directory the package is installyed.")

;; * Connect & Create

(defun org-files-db--database-connect (&optional path)
  "Connect to the database at PATH. If it doesn't exist it will be created.
If the `org-files-db--database-version' doesn't match the database is recreated
as well."
  (let* ((path (or path org-files-db-path-of-database))
         (name org-files-db--database-process-name)
         (schema org-files-db--database-schema-path)
         (executable org-files-db-sqlite-executable)
         (version org-files-db--database-version)
         (timeout 5000)
         process version-matches)
    ;; Connect if db exists.
    (when (file-exists-p path)
      (setq process (org-files-db--sqlite-process-start path name
                                                        executable t t timeout))
      (setq version-matches (= (org-files-db--sqlite-get-user-version process)
                               version)))
    ;; Create or recreate the db if needed.
    (setq org-files-db--database-process
          (if (and process version-matches)
              process
            (cond ((not process) (message "Creating new database"))
                  ((not version-matches)
                   (message (concat "Recreating database as the "
                                    "version doesn't match"))))
            (org-files-db--sqlite-create-database path schema name executable
                                                  version t t timeout)))))

(defun org-files-db--database-disconnect ()
  "Disconnect from the database."
  (org-files-db--sqlite-process-delete org-files-db--database-process))

;; * Insert

;; * Update

;; * Delete

;; * Select

;; * Footer

(provide 'org-files-db-database)

;;; org-files-db-database.el ends here
