;;; test-org-files-db.el --- Tests for org-files-db -*- lexical-binding:t -*-

;; Package-Requires: ((buttercup))

;;; Commentary:

;; Tests to check if the database functions are working.

;;; Code:

;;;; * Requirements

(require 'buttercup)
(require 'ert)

(require 'org-files-db)

;;;; * Helpers

;;;; * Build

(describe "A suite"
  (it "contains a spec with an expectation"
    (expect t :to-be t)))

(provide 'test-org-files-db)

;;; test-org-files-db.el ends here
