;;; org-files-db-db.el --- Sqlite Database for Org Headings -*- lexical-binding: t -*-

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

;; Build the database to

;;; Code:

;;;; * Requirements

(require 'seq)
(require 'sqlite)

(require 'org-files-db-core)
(require 'org-files-db-database)

;;;; * Constants & Variables

(defconst org-files-db-db--schema-file
  (expand-file-name "sql/db-schema.sql" org-files-db--load-dir)
  "File with the schema to build the database.")

(defconst org-files-db-db--user-version 1
  "The current version of the database.
If the database version changes it will be rebuilt from scratch.")

(defvar org-files-db-db--object nil
  "Holds the sqlite object.")

;;;; * Build

;;;; * Open & Close

(defun org-files-db-db--open ()
  "Open the Org files database and return the sqlite object.
If the database doesn't exist or the user-version has changed the
data is (re)built from scratch."
  (setq org-files-db-db--object
        (org-files-db--sqlite-open
         org-files-db-file
         org-files-db-db--user-version
         org-files-db-db--schema-file)))

(defun org-files-db-db--close (args)
  "Close the Org files database."
  (setq org-files-db-db--object
        (org-files-db--sqlite-close org-files-db-db--object)))

;;;; * Insert

;; TODO is nil converted to NULL?

(defun org-files-db-db--insert-directory (directory updated)
  "Insert a DIRECTORY into the directories table in the database.
 Sets the UPDATED timestamp, in seconds since the epoch.

Example:
  (org-files-db-db--insert-directory \"/home/user1/org-files\" 1609459200)"
  (sqlite-execute org-files-db-db--object
                  "INSERT INTO directories (directory, updated) VALUES(?, ?)"
                  (list directory updated)))

(defun org-files-db-db--insert-file (directory-id filename udpated inode mtime
                                                  size)
  "Insert a file into the files table in the database.

Arguments:
- DIRECTORY-ID: ID of the directory in which the file is located. The ID can be
  retrieved from the directories table.
- FILENAME: Absolute filename name of the file.
- UDPATED: Date and time the file was last updated, in seconds since the
  epoch.
- INODE: Inode number of the file.
- MTIME: Time of the file's last data modification, in seconds since the
  epoch.
- SIZE: Size of the file in bytes.

Example:
  (org-files-db-db--insert-file
   1, \"/home/user1/org-files/my-file.txt\",
   1609459200, 6957347, 1609459200, 1024)"
  (sqlite-execute
   org-files-db-db--object
   "INSERT INTO files (directory_id, filename, updated, inode, mtime, size) \
VALUES(?, ?, ?, ?, ?, ?)"
   (list directory-id filename udpated inode mtime size)))

(defun org-files-db-db--insert-heading (file-id level point full-text
                                                priority todo-keyword title
                                                statistic-cookies scheduled
                                                deadline closed parent-id)
  "Insert a heading into the headings table in the database.
An artifical \"heading\" at file level is stored to capture the files metadata.

Arguments:
- FILE-ID: ID of the file in which the heading is located. The ID can be
  retrieved from the files table.
- LEVEL: Level of the heading. As the file itself can also have metadata, it is
  stored as a level 0 heading.
- FULL-TEXT: Full line text of heading including stars, todo keywords etc.
- TITLE: Title of the heading, just the text of the heading with starts etc.
- PARENT-ID: ID of the parent heading.
The other arguments are self-explanatory POINT, PRIORITY, TODO-KEYWORD,
STATISTIC-COOKIES, SCHEDULED, DEADLINE, CLOSED.

Example:
  (org-files-db-db--insert-heading
   1 1 50 \"* TODO [#A] Headline [0//1]\" \"A\" \"TODO\"
   \"Headline\" \"[0/1]\" nil nil nil 2)"
  ;; TODO This functions has to extract some values from the headline text.
  (sqlite-execute
   org-files-db-db--object
   "INSERT INTO headings (file_id, level, point, full_text, priority, \
todo_keyword, title, statistic_cookies, scheduled, deadline, closed, parent_id)\
 VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
   (list file-id level point full-text priority todo-keyword title
         statistic-cookies scheduled deadline closed parent-id)))

(defun org-files-db-db--insert-tag (heading-id tag)
  "Insert a TAG into the tags table in the database.
The TAG is stored for the heading with HEADING-ID. This value can be retrieved
from the headings table.

Example:
  (org-files-db-db--insert-tag 3 \"tag\")"
  (sqlite-execute
   org-files-db-db--object
   "INSERT INTO tags (heading_id, tag) VALUES (?, ?)"
   (list heading-id tag)))

(defun org-files-db-db--insert-property (heading-id property value)
  ""
  )

(defun org-files-db-db--insert-link (file-id point full-link)
  "Insert a link into the links table in the database.

Arguments:
- FILE-ID: ID of the file in which the link is located. The ID can be retrieved
  from the files table.
- POINT: Position of the link in the file.
- FULL-LINK: Complete text of the link.

Example:
  \"[[info:org#Link Format][org#Link Format]]\""
  ;; TODO This functions has to extract type and so on from link text.
  (let* ((type (fun full-link))
         (link (fun full-link))
         (description (fun full-link)))
    (sqlite-execute
     org-files-db-db--object
     "INSERT INTO tags (heading_id, tag) VALUES (?, ?)"
     (list file-id point full-link type link description))))

;;;; * Update

(defun org-files-db-db--update-directory (args)
  ""

  )

(defun org-files-db-db--update-file (args)
  ""

  )

(defun org-files-db-db--update-heading (args)
  ""

  )

(defun org-files-db-db--update-tag (args)
  ""

  )

(defun org-files-db-db--update-property (args)
  ""

  )

(defun org-files-db-db--update-link (args)
  ""

  )

;;;; * Delete

(defun org-files-db-db--delete-directory (args)
  ""

  )

(defun org-files-db-db--delete-file (args)
  ""

  )

(defun org-files-db-db--delete-heading (args)
  ""

  )

(defun org-files-db-db--delete-tag (args)
  ""

  )

(defun org-files-db-db--delete-property (args)
  ""

  )

(defun org-files-db-db--delete-link (args)
  ""

  )

;;;; * Queries

;;;; * Footer

(provide 'org-files-db-db)

;;; org-files-db.el ends here
