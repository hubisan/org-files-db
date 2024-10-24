;;; org-files-db-parse.el --- Org files parsing for org-files-db -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Daniel Hubmann

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

;; This file provides functions to parse Org files and extract relevant data.
;; It is part of the org-files-db package, which stores Org file data
;; in a SQLite database.

;;; Code:

;;;; * Requirements

(require 'org)
(require 'org-element)

(require 'org-files-db-core)

;;;; * Global Constants & Variables

;;;; * Auxiliary Functions

;;;; * Files

(defun org-files-db-parse--parse-file (filename)
  "Parse the Org headings including level 0 in FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (delay-mode-hooks
      (let ((org-inhibit-startup t)
            (org-agenda-files nil)
            (org-timestamp-formats '("%Y-%m-%d %a" . "%Y-%m-%d %a %H:%M"))
            (org-use-tag-inheritance nil)
            (org-use-property-inheritance nil))
        ;; Org throws an error if this is not set to 8.
        (setq-local tab-width 8)
        (org-mode)
        (hack-local-variables)
        (let ((tree (org-element-parse-buffer)))
          (list
           (org-files-db-parse--parse-buffer tree)
           (org-files-db-parse--parse-links tree)))))))

;;;; * Buffer

(defun org-files-db-parse--parse-buffer (tree)
  "Parse the current buffer.
The buffer parse TREE is needed."
  (let* ((lvl-0 (org-files-db-parse--parse-level-0 tree))
         (headings (org-files-db-parse--parse-headings tree)))
    (push lvl-0 headings)))

;;;; * Headings

(defun org-files-db-parse--parse-level-0 (tree)
  "Parse the level 0 in current buffer.
The TREE is used to get the keywords."
  (let* ((keywords (org-element-map tree 'keyword
                     (lambda (property)
                       (list (org-element-property :key property)
                             (org-element-property :value property)))))
         (title-raw (cadr (assoc "TITLE" keywords)))
         ;; Use the TITLE or the filename without extension.
         (title-text (if title-raw
                         (substring-no-properties
                          (org-sort-remove-invisible title-raw))
                       (file-name-sans-extension (buffer-file-name)))))
    (list
     :level 0
     :begin 0
     :title title-text
     :title-raw title-raw
     :keywords keywords
     :tags (split-string (cadr (assoc "FILETAGS" keywords)) ":" t)
     :properties (org-files-db-parse--get-local-properties (point-min)))))

(defun org-files-db-parse--parse-headings (tree)
  "Parse the headings in the current buffer.
The parse TREE is used if possible to extract the metadata."
  (org-element-map tree 'headline
    (lambda (node)
      (let* ((title-raw (org-element-property :raw-value node))
             ;; Convert links to text and remove statistics-cookie.
             ;; Taken from `org--get-outline-path-1'.
             (statistics-cookie (org-element-map node 'statistics-cookie
                                  (lambda (cookie)
                                    (org-element-property
                                     :value cookie))
                                  nil t t))
             (title-text (org-trim
                          (substring-no-properties
                           ;; This also removes emphasis markers.
                           ;; Used `org-link-display-format' before.
                           (org-sort-remove-invisible
                            (if statistics-cookie
                                (replace-regexp-in-string
                                 (regexp-quote (concat " " statistics-cookie))
                                 ""
                                 title-raw
                                 t t)
                              title-raw)))))
             (priority (org-element-property :priority node))
             (priority (when (characterp priority) (char-to-string priority))))
        (list
         :level (org-element-property :level node)
         :begin (org-element-property :begin node)
         :title title-text
         :title-raw title-raw
         :priority priority
         :todo-keyword (org-element-property :todo-keyword node)
         :todo-type (org-element-property :todo-type node)
         :archivedp (org-element-property :archivedp node)
         :footnote-section-p (org-element-property :footnote-section-p node)
         :tags (org-element-property :tags node)
         :scheduled (org-element-property :scheduled node)
         :deadline (org-element-property :deadline node)
         :closed (org-element-property :closed node)
         :properties (org-files-db-parse--get-local-properties node))))))

;;;; * Keywords

(defun org-files-db-parse--get-level-0-keywords (parse-tree)
  "Return a list of the Org keywords at level 0 (file level) for PARSE-TREE."
  (org-element-map parse-tree 'keyword
    (lambda (property)
      (list (org-element-property :key property)
            (org-element-property :value property)))))

;;;; * Properties

(defun org-files-db-parse--get-local-properties (epom)
  "Get the local properties of EPOM, which is an element, point or marker.
Got this from `org-entry-properties'. I only want the local properties
without inherited category. When trying to get the properties from the
parse tree directly, it included properties of children. Found no solution to
only get the headings properties."
  (org-with-point-at epom
    (when (and (derived-mode-p 'org-mode)
               (org-back-to-heading-or-point-min t))
      (let ((range (org-get-property-block (point)))
            props)
        (when range
          (let ((end (cdr range)) seen-base)
            (goto-char (car range))
            (while (re-search-forward org-property-re end t)
              (let* ((key (upcase (match-string-no-properties 2)))
                     (extendp (string-match-p "\\+\\'" key))
                     (key-base (if extendp (substring key 0 -1) key))
                     (value (match-string-no-properties 3)))
                (cond
                 ((member-ignore-case key-base org-special-properties))
                 (extendp
                  (setq props
                        (org--update-property-plist key value props)))
                 ((member key seen-base))
                 (t (push key seen-base)
                    (let ((p (assoc-string key props t)))
                      (if p (setcdr p (concat value " " (cdr p)))
                        (push (cons key value) props)))))))))
        props))))

;;;; * Links

(defun org-files-db-parse--parse-links (tree)
  "Parse the links with using the parse TREE."
  (org-element-map tree 'link
    (lambda (link)
      (let* ((path (org-element-property :path link))
             (type (org-element-property :type link))
             (path-absolute (when (string-equal type "file")
                              (expand-file-name path))))
        (list
         :type type
         :begin (org-element-property :begin link)
         :path path
         :path-absolute path-absolute
         :description (substring-no-properties
                       (org-element-interpret-data (org-element-contents link)))
         :format (org-element-property :format link)
         :raw-link (org-element-property :raw-link link)
         :search-option (org-element-property :search-option link))))))

;;;; * Footer

(provide 'org-files-db-parse)

;;; org-files-db-parse.el ends here
