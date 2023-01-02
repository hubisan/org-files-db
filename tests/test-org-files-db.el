;;; test-org-files-db.el --- Tests for org-files-db -*- lexical-binding:t -*-

;; Package-Requires: ((buttercup))

;;; Commentary:

;; If you want to split your tests into multiple files just use another file
;; named test-*.el, *-test.el or *-tests.el

;;; Code:

;;;; * Requirements

(require 'buttercup)
(require 'ert)

(require 'org-files-db)

;;;; * Helpers

;;;; * Heading1

(describe "A suite"
  (it "contains a spec with an expectation"
    (expect t :to-be t)))

(provide 'test-org-files-db)

;;; test-org-files-db.el ends here
