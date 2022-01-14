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
If the database version changes it will be rebuilt from scratch.")

(defvar org-files-db--db-connection nil
  "Database connection to org-files-db database.")

;; SQLite Dataypes: integer, real, text, blob
(defconst org-files-db--db-schema
  '((db
     [(version integer :not-null)])
    (directories
     [(directory text :unique :primary-key)
      ;; Updated and mtime stored as seconds since the epoch.
      (updated integer :not-null)
      (mtime integer :not-null)
      (size integer :not-null)])
    (files
     [(filename text :unique :primary-key)
      (directory text :not-null)
      (title text)]
     (:foreign-key [directory] :references directories [directory] :on-delete :cascade))
    (headings
     ([(id integer :not-null :primary-key)
       (file text :not-null)
       (level integer :not-null)
       (position integer :not-null)
       (priority text)
       (todoKeyword text)
       (title text)
       (statisticCookies text)
       ;; Store as float to be able to store date and time.
       (scheduled real)
       (deadline real)
       (closed real)
       (parentId integer)]
      (:foreign-key [file] :references files [file] :on-delete :cascade)
      (:foreign-key [parentId] :references headings [id] :on-delete :cascade)))
    (tags
     ([(headingId integer :not-null)
       (tag text :not-null)]
      (:primary-key [headingId, tag])
      (:foreign-key [headingId] :references headings [id] :on-delete :cascade)))
    (properties
     ([(headingId integer :not-null)
       (property text :not-null)
       (value text)]
      (:primary-key [headingId, property])
      (:foreign-key [headingId] :references headings [id] :on-delete :cascade)))
    (links
     ([(fileId integer :not-null)
       (position integer :not-null)
       (full-link text :not-null)
       (type text)
       (link text :not-null)
       (description text)]
      (:primary-key [fileId, position])
      (:foreign-key [file] :references files [file] :on-delete :cascade)))))

;; * Build

;; * Create

;; (defun org-files-db--db-exists-p (path)
;;   "Check if the database file with path exists."
;;   ;; TODO
;;   )

;; * Connection

(defun org-files-db--db-open-connection (path)
  "Open connection to the database at PATH and return it.
this also enables foreign keys."
  (let ((conn (emacsql-sqlite path)))
    (emacsql conn [:pragma (= foreign_keys on)])
    conn))

(defun org-files-db--db-close-connection (conn)
  "Close the connection CONN to the database."
  (emacsql-close conn))

;; * Insert

;; Insert directory
;; Insert file
;; Insert property
;; Insert tag
;; Insert link

;; * Update

;; Update directory

;; * Drop

;; Drop directory
;; Drop file

;; * Select

;; * Footer

(provide 'org-files-db-database)

;;; org-files-db-database.el ends here
