;;; helpers.el --- Helper functions -*- lexical-binding: t -*-

;;; Commentary:

;; TODO

;;; Code:

;; * Files

(defun helpers-file-read-contents (path)
  "Return the contents file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

;; * Footer

(provide 'helpers)

;;; helpers.el ends here
