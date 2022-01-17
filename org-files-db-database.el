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
  "Database connection to org-files-db database.")

(defconst org-files-db--db-schema
  ;; All the directories in which the org-files are parsed.
  '((directories
     [(directory text :not-null :primary-key)
      ;; Updated, mtime and size are used to make sure the directory is not
      ;; dirty. Updated and mtime stored as seconds since the epoch.
      (updated integer :not-null)
      (mtime integer :not-null)
      (size integer :not-null)])
    ;; Metadata of the org files in those directories.
    (files
     [(filename text :not-null :primary-key)
      (directory text :not-null)
      ;; Updated, mtime and size are used to make sure the file is not dirty.
      ;; Updated and mtime stored as seconds since the epoch.
      (updated integer :not-null)
      (mtime integer :not-null)
      (size integer :not-null)
      ;; The title keyword if there is any.
      (title text)]
     (:foreign-key [directory] :references directories [directory] :on-delete :cascade))
    ;; Metadata of the headings in the org files.
    ;; Level 0 is used for file level. This is needed to store file level
    ;; properties and tag.
    (headings
     ([(id integer :not-null :primary-key)
       (file text :not-null)
       (level integer :not-null)
       (position integer :not-null)
       (priority text)
       (todo_keyword text)
       (title text)
       (statistic_cookies text)
       ;; Store planning info as float to be able to store date and time.
       (scheduled real)
       (deadline real)
       (closed real)
       (parent_id integer)]
      (:unique [file position])
      (:foreign-key [file] :references files [file] :on-delete :cascade)
      (:foreign-key [parent_id] :references headings [id] :on-delete :cascade)))
    ;; Tags per heading.
    (tags
     ([(heading_id integer :not-null)
       (tag text :not-null)]
      (:primary-key [heading_id tag])
      (:foreign-key [heading_id] :references headings [id] :on-delete :cascade)))
    ;; Properties per heading.
    (properties
     ([(heading_id integer :not-null)
       (property text :not-null)
       (value text)]
      (:primary-key [heading_id property])
      (:foreign-key [heading_id] :references headings [id] :on-delete :cascade)))
    ;; Links in the files.
    (links
     ([(file integer :not-null)
       (position integer :not-null)
       (full_link text :not-null)
       (type text)
       (link text :not-null)
       (description text)]
      (:primary-key [file position])
      (:foreign-key [file] :references files [file] :on-delete :cascade))))
  "The schema that is used for the database.")

(defconst org-files-db--db-indices
  '((headings-title-id headings [title]))
  "The indices used in the database.
No indices are create if this is set to nil")

;; * Build

;; SQLite Dataypes: integer, real, text, blob

(defun org-files-db--db-initialize (path)
  "Initialize the database at PATH.
Use the `org-files-db--db-schema' to create the tables and also create the
INDICES taken from `org-files-db--db-indices'. If the database exists the
existing tables will be dropped. The user_version is set to
`org-files-db--db-version'. This sets the `org-files-db--db-connection'."
  ;; Create the file at path. It will be overwritten if it already exists.
  (org-files-db--db-create-db-file path)
  (let* ((conn (org-files-db--db-open-connection path))
         (schema org-files-db--db-schema)
         (indices org-files-db--db-indices)
         (version org-files-db--db-version))
    ;; Store the connection.
    (setq org-files-db--db-connection conn)
    ;; Create the tables and the indices.
    (org-files-db--db-create-tables conn schema)
    (org-files-db--db-create-indices conn indices)
    ;; Set the user version.
    (org-files-db--db-set-user-version conn version)))

(defun org-files-db--db-exists-p (path)
  "Check if the database file with PATH exists."
  (file-exists-p path))

(defun org-files-db--db-create-db-file (path)
  "Create the database file at PATH.
Will overwrite existing files and creates parent directories if needed."
  (make-empty-file path t))

(defun org-files-db--db-set-user-version (db version)
  "Set the user_version to VERSION for the open DB connection."
  (emacsql db (format "PRAGMA user_version = %s" version)))

(defun org-files-db--db-get-user-version (db)
  "Get the user_version of the open DB connection."
  (caar (emacsql db "PRAGMA user_version")))

;; * Connection

(defun org-files-db--db-open-connection (path)
  "Open connection to the database at PATH and return it.
This also enables foreign keys."
  (let ((connection (emacsql-sqlite path)))
    (emacsql connection [:pragma (= foreign_keys on)])
    connection))

(defun org-files-db--db-close-connection (db)
  "Close the DB connection."
  (emacsql-close db))

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

(defun org-files-db--insert-directory (db dir updated mtime size)
  "Insert directory DIR into the directory table in the connected DB.
Also store the current time UPDATED, the MTIME (modification time) and the SIZE
in bytes. Times are stored as seconds since the epoch."
  (emacsql db [:insert :into directories :values $v1]
           (vector dir updated mtime size )))

;; Insert file

(defun org-files-db--insert-file (db file dir updated mtime size title)
  "Insert FILE into the files table in the connected DB.
Also store the current time UPDATED, the MTIME (modification time) and the SIZE
in bytes. Times are stored as seconds since the epoch. If there is a title in
the org file it is stored as well."
  (emacsql db [:insert :into files :values $v1]
           (vector file dir updated mtime size title)))

;; Insert heading

(defun org-files-db--insert-heading (db file level pos prio todo title cookies
scheduled deadline closed parent-id)
  "Insert PROPERTY and its VALUE into the properties table in the connected DB.
Also stores ID HEADINGID of the referenced the heading."
  (emacsql db [:insert :into files :values $v1]
           (vector file level pos prio todo title cookies scheduled deadline
           closed parent-id)))

;; Insert property

(defun org-files-db--insert-property (db heading-id property value)
  "Insert PROPERTY and its VALUE into the properties table in the connected DB.
Also stores ID HEADINGID of the referenced the heading."
  (emacsql db [:insert :into files :values $v1]
           (vector heading-id property value)))

;; Insert tag


;; Insert link

;; * Update

;; Update directory

;; * Delete

;; Update directory

;; * Drop

;; Drop directory
;; Drop file

;; * Select

;; * Footer

(provide 'org-files-db-database)

;;; org-files-db-database.el ends here
