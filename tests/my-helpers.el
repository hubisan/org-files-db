;;; my-helpers.el --- Helper functions -*- lexical-binding: t -*-

;;; Commentary:

;; Helper functions for all tests.

;;; Code:

;;;; * Files

(defun my-helpers-file-read-contents (path)
  "Return the contents file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

;;;; * Footer

(provide 'my-helpers)

;;; helpers.el ends here
