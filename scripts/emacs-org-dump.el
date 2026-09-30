;;; emacs-org-dump.el --- Dump Org facts from Emacs as JSON  -*- lexical-binding: t -*-

;; Reference oracle for the orgfdb parser (issue #4).  Local use only.
;;
;; Usage:
;;   emacs --batch -Q -l scripts/emacs-org-dump.el --eval '(ofdb-dump "FILE")'
;; or via scripts/emacs-oracle.py, which wraps this.
;;
;; Safety: the file is only parsed.  No file-local variables, dir-local
;; variables, Babel, link following, diary sexps or #+SETUPFILE.

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'subr-x)

;; Never evaluate anything from the file.
(setq enable-local-variables nil
      enable-dir-local-variables nil
      enable-local-eval nil
      org-element-use-cache nil
      org-inhibit-startup t
      org-mode-hook nil
      org-link-elisp-confirm-function (lambda (&rest _) nil)
      org-link-shell-confirm-function (lambda (&rest _) nil)
      org-confirm-babel-evaluate t
      org-agenda-include-diary nil)

(defun ofdb--obj (&rest kv)
  "Build a JSON object from alternating string keys and values."
  (let ((h (make-hash-table :test 'equal)))
    (while kv (puthash (pop kv) (pop kv) h))
    h))

(defun ofdb--str (s)
  (if s (substring-no-properties s) :null))

(defun ofdb--vec (list) (vconcat list))

(defun ofdb--props-obj (alist)
  "Turn ALIST of (KEY . VALUE) into a JSON object with upcased keys."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (p alist)
      (puthash (upcase (substring-no-properties (car p)))
               (substring-no-properties (or (cdr p) ""))
               h))
    h))

(defun ofdb--entry-props (pos &optional file-level)
  "Direct standard properties at POS.
Emacs always reports a synthetic CATEGORY (from the file name).  Drop it
unless the drawer, or for FILE-LEVEL a #+CATEGORY keyword, declares it."
  (save-excursion
    (goto-char pos)
    (let* ((case-fold-search t)
           (props (org-entry-properties nil 'standard))
           (block (org-get-property-block))
           (text (and block (buffer-substring-no-properties (car block) (cdr block))))
           (has-cat (or (and text (string-match-p "^[ \t]*:CATEGORY\\+?:" text))
                        (and file-level
                             (save-excursion
                               (goto-char (point-min))
                               (re-search-forward "^[ \t]*#\\+CATEGORY:" nil t))))))
      (if has-cat props (assoc-delete-all "CATEGORY" (copy-alist props))))))

(defun ofdb--links (node)
  (let (out)
    (org-element-map node 'link
      (lambda (l)
        (let ((cb (org-element-property :contents-begin l))
              (ce (org-element-property :contents-end l))
              (b (org-element-property :begin l))
              (e (- (org-element-property :end l)
                    (or (org-element-property :post-blank l) 0))))
          (push (ofdb--obj
                 "type" (ofdb--str (org-element-property :type l))
                 "path" (ofdb--str (org-element-property :path l))
                 "search_option" (ofdb--str (org-element-property :search-option l))
                 "format" (symbol-name (org-element-property :format l))
                 "description" (if (and cb ce)
                                   (buffer-substring-no-properties cb ce)
                                 :null)
                 "raw" (buffer-substring-no-properties b e))
                out))))
    (nreverse out)))

(defun ofdb--ts-raw (ts)
  (if ts (ofdb--str (org-element-property :raw-value ts)) :null))

(defun ofdb--heading (h)
  (let* ((title (org-element-property :title h))
         (section (let ((c (car (org-element-contents h))))
                    (and c (eq (org-element-type c) 'section) c)))
         (prio (org-element-property :priority h))
         (links (append (ofdb--links title)
                        (and section (ofdb--links section)))))
    (ofdb--obj
     "level" (org-element-property :level h)
     "title" (ofdb--str (org-element-property :raw-value h))
     "todo" (ofdb--str (org-element-property :todo-keyword h))
     "todo_type" (pcase (org-element-property :todo-type h)
                   ('todo "open") ('done "closed") (_ :null))
     "line" (line-number-at-pos (org-element-property :begin h))
     "priority" (if prio (char-to-string prio) :null)
     "tags" (ofdb--vec (mapcar #'substring-no-properties
                               (org-element-property :tags h)))
     "archived" (if (org-element-property :archivedp h) t :false)
     "commented" (if (org-element-property :commentedp h) t :false)
     "properties" (ofdb--props-obj
                   (ofdb--entry-props (org-element-property :begin h)))
     "scheduled" (ofdb--ts-raw (org-element-property :scheduled h))
     "deadline" (ofdb--ts-raw (org-element-property :deadline h))
     "closed" (ofdb--ts-raw (org-element-property :closed h))
     "links" (ofdb--vec links))))

(defun ofdb-dump (file)
  "Print the JSON fact dump of Org FILE to stdout."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents file))
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*#\\+SETUPFILE:" nil t)
      (error "Refusing to parse a file with #+SETUPFILE"))
    (delay-mode-hooks (org-mode))
    (org-set-regexps-and-options)
    (let* ((tree (org-element-parse-buffer))
           (first (car (org-element-contents tree)))
           (preamble (and first (eq (org-element-type first) 'section) first))
           (keywords nil) (headings nil)
           (done org-done-keywords))
      (org-element-map tree 'keyword
        (lambda (k)
          (push (vector (upcase (org-element-property :key k))
                        (ofdb--str (org-element-property :value k)))
                keywords)))
      (org-element-map tree 'headline
        (lambda (h) (push (ofdb--heading h) headings)))
      (setq coding-system-for-write 'utf-8-unix)
      (princ
       (json-serialize
        (ofdb--obj
         "file" (ofdb--obj
                 "keywords" (ofdb--vec (nreverse keywords))
                 "file_tags" (ofdb--vec (mapcar #'substring-no-properties
                                                org-file-tags))
                 "todo_keywords"
                 (ofdb--vec
                  (mapcar (lambda (k)
                            (vector (substring-no-properties k)
                                    (if (member k done) "closed" "open")))
                          org-todo-keywords-1))
                 "properties" (ofdb--props-obj
                               (append org-keyword-properties
                                       ;; With a heading on line 1, point-min
                                       ;; is that heading, not the file level.
                                       (and (save-excursion
                                              (goto-char (point-min))
                                              (org-before-first-heading-p))
                                            (ofdb--entry-props (point-min) t))))
                 "links" (ofdb--vec (and preamble (ofdb--links preamble))))
         "headings" (ofdb--vec (nreverse headings))))))))

(provide 'emacs-org-dump)
;;; emacs-org-dump.el ends here
