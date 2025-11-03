;;; org-files-db.el --- Store data from Org files in a SQLite database -*- lexical-binding: t -*-

;; Copyright (C) 2024 Daniel Hubmann <hubisan@gmail.com>

;; Author: Daniel Hubmann <hubisan@gmail.com>
;; Maintainer: Daniel Hubmann <hubisan@gmail.com>
;; URL: https://github.com/hubisan/org-files-db
;; Keywords: outlines
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.7.13"))

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


;; The org-files-db package allows you to store and manage data from Org files
;; (headings, tags, properties, links) in a SQLite database. It supports the
;; extraction of relevant information from specified directories or individual
;; Org files.

;; The primary goal of org-files-db is to make it easy to interact with large
;; collections of Org files by leveraging the power of a relational database.
;; With the data stored in SQLite, you can perform complex queries, filter
;; information, and integrate seamlessly with other tools and workflows.

;; Parsing Org files and inserting data into the database is done
;; asynchronously, ensuring a non-blocking and responsive experience.

;; It detect changes in files periodically and updates the database
;; automatically to keep your data synchronized. Changes made to Org files with
;; Emacs are updated instantly.

;;; Code:

;; Use this to add the files to the load path:
;; (let ((default-directory "~/projects/coding/org-files-db"))
;;   (normal-top-level-add-to-load-path '("."))
;;   (normal-top-level-add-subdirs-to-load-path))

;;;; * Requirements

(require 'org-files-db-core)
(require 'org-files-db-database)
(require 'org-files-db-parse)

;;;; * Footer

(provide 'org-files-db)

;;; org-files-db.el ends here
