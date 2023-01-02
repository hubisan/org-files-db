;;; org-files-db-fts.el --- Full text search -*- lexical-binding: t -*-

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

;; Implementation of full text search with a separate database. SQLite has a
;; built-int full text search.

;; TODO

;;; Code:

;;;; * Requirements

(require 'org-files-db-core)
(require 'org-files-db-database)

;;;; * Variables

(defvar org-files-db--fts-process nil
  "Process that runs the SQLite3 interative shell.")

(defconst org-files-db--fts-process-name "org-files-db"
  "The name for the process. Is also used to name the buffer.")

(defvar org-files-db--fts-sqlite-output nil
  "The output of the SQLite3 interactive shell.")

;;;; * Main

;;;; * Footer

(provide 'org-files-db-fts)

;;; org-files-db-fts.el ends here
