;;; orgfdb-org-inspect.el --- Org inspection helpers -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'ob-core)

(defun orgfdb-parse-named-org-src-block (name &optional display-buffer-p)
  "Pretty-print org-element parse tree for Org source block named NAME.

When DISPLAY-BUFFER-P is nil, return the pretty-printed parse tree
as a string.  This is useful for Org Babel blocks that should print
the result directly into the Org buffer.

When DISPLAY-BUFFER-P is non-nil, write the result to
`*orgfdb-org-element-parse*' and display that buffer."
  (interactive
   (list
    (read-string "Org source block name: ")
    current-prefix-arg))
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward
             (format "^[ \t]*#\\+name:[ \t]*%s[ \t]*$" (regexp-quote name))
             nil t)
      (error "Could not find source block named %s" name))
    (forward-line 1)
    (let* ((info (org-babel-get-src-block-info))
           (lang (nth 0 info))
           (body (nth 1 info)))
      (unless (string= lang "org")
        (error "Expected an org source block, got %s" lang))
      (let ((result
             (with-temp-buffer
               (org-mode)
               (insert body)
               (goto-char (point-min))
               (org-element-parse-buffer))))
        (if display-buffer-p
            (with-current-buffer (get-buffer-create "*orgfdb-org-element-parse*")
              (erase-buffer)
              (emacs-lisp-mode)
              (insert result)
              (goto-char (point-min))
              (display-buffer (current-buffer)))
          result)))))

(provide 'orgfdb-org-inspect)

;;; orgfdb-org-inspect.el ends here
