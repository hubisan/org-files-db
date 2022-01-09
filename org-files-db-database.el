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

;; * Variables

(defconst org-files-db--db-version 1
  "The version of the database.
If the database version changes it will be rebuilt from scratch.")

(defvar org-files-db--db-connection nil
  "Database connection to org-files-db database.")

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
      (directory text :not-null)]
     (:foreign-key [directory] :references directories [directory] :on-delete :cascade))
    (headings
     ([(id :not-null :primary-key)
       (file :not-null)
       (level :not-null)
       (pos :not-null)
       todo
       priority
       (scheduled text)
       (deadline text)
       title
       properties
       olp]
      (:foreign-key [file] :references files [file] :on-delete :cascade)))

    (aliases
     ([(node-id :not-null)
       alias]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (citations
     ([(node-id :not-null)
       (cite-key :not-null)
       (pos :not-null)
       properties]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (refs
     ([(node-id :not-null)
       (ref :not-null)
       (type :not-null)]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (tags
     ([(node-id :not-null)
       tag]
      (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))

    (links
     ([(pos :not-null)
       (source :not-null)
       (dest :not-null)
       (type :not-null)
       (properties :not-null)]
      (:foreign-key [source] :references nodes [id] :on-delete :cascade)))))

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
