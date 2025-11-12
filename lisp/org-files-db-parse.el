;;; org-files-db-parse.el --- Org file parser for Org Files DB -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Daniel Hubmann

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

;; This module provides parsing functions for Org files.
;; It converts Org content into structured Lisp data suitable for
;; indexing in the database.

;; Parsing focuses on:
;; - Headings (titles, levels, TODO keywords, tags)
;; - Properties and drawers
;; - Scheduling and deadlines
;; - Links and file references
;; - Optional metadata (#+TITLE, #+CATEGORY, etc.)

;;; Code:

;;; Requirements

(require 'org)
(require 'org-element)

;;; Constants & Variables

;;; Utility

(defun org-files-db-parse--valid-org-file-p (filename)
  "Return non-nil if FILENAME is a valid Org file that can be parsed."
  )

;;; Wrapper

(defun org-files-db-parse-file (filename)
  "Parse the Org file with FILENAME and return structured data.
The returned structure is suitable for database insertion.
Signals an error if the file does not exist or cannot be parsed."
  )

(defun org-files-db-parse-buffer ()
  "Parse the current buffer as an Org file and return structured data.
Intended for internal use when the buffer is already visiting an Org file."
  )

;;; Parsing

(defun org-files-db-parse--collect-elements (parsed opts)
  "Walk PARSED Org data and collect all relevant elements according to OPTS.
Return a flat list of plists, each representing a headline, link, timestamp, etc."
  (let ((result '()))
    ;; Ein einziger Durchlauf durch den gesamten Baum
    (org-element-map parsed '(headline link timestamp keyword)
      (lambda (el)
        (pcase (org-element-type el)

          ;; Headline
          ('headline
           (push (org-files-db-parse--headline el opts) result))

          ('keyword
           (push (org-files-db-parse--headline el opts) result))

          ;; Links (optional)
          ('link
           (when (plist-get opts :include-links)
             (push (org-files-db-parse--link el) result)))

          ;; Timestamps (optional)
          ('timestamp
           (when (plist-get opts :include-timestamps)
             (push (org-files-db-parse--timestamp el) result)))
          ))

      ;; recurse into all sub-elements of headlines etc.
      nil nil)

    (nreverse result)))

(defun org-files-db-parse--headline (el opts)
  "Parse a headline EL into a plist structure."
  (let* ((title (org-element-property :raw-value el))
         (todo  (org-element-property :todo-keyword el))
         (level (org-element-property :level el))
         (tags  (when (plist-get opts :include-tags)
                  (org-element-property :tags el)))
         (props (when (plist-get opts :include-properties)
                  (org-files-db-parse--properties el)))
         (timestamps (when (plist-get opts :include-timestamps)
                       (org-files-db-parse--timestamps el)))
         (begin (org-element-property :begin el)))
    `(:type "headline"
      :title ,title
      :todo ,todo
      :level ,level
      :tags ,tags
      :properties ,props
      :timestamps ,timestamps
      :pos ,begin)))



(defun org-files-db-parse--file-keywords ()
  "Extract file-level metadata (#+TITLE, #+CATEGORY, etc.) from the current buffer."
  )

(defun org-files-db-parse--headline (element)
  "Parse a single Org ELEMENT of type headline and return structured data.
The return value should include title, level, TODO keyword, tags,
properties, and scheduling/deadline info."
  )

(defun org-files-db-parse--properties (element)
  "Extract properties from an Org ELEMENT (usually a headline)."
  )

(defun org-files-db-parse--tags ()
  ""
  )

(defun org-files-db-parse--timestamp (el)
  "Return a plist for a timestamp element."
  (let ((raw (org-element-property :raw-value el))
        (ts-type (org-element-property :type el)))
    `(:type "timestamp"
      :timestamp-type ,ts-type
      :raw ,raw)))

(defun org-files-db-parse--link (el)
  "Return a plist representing a link element."
  (let ((type (org-element-property :type el))
        (path (org-element-property :path el))
        (desc (org-element-contents el)))
    `(:type "link"
      :link-type ,type
      :path ,path
      :description ,(when desc (org-no-properties (org-element-interpret-data desc))))))

;;; Provide

(provide 'org-files-db-parse)

;;; org-files-db-parse.el ends here
