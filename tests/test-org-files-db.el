;;; test-org-files-db.el --- Tests  -*- lexical-binding:t -*-

;;; Requirements

(require 'buttercup)
(require 'ert)
(require 'with-simulated-input)

(require 'test-helper)

(require 'org-files-db)

;;; Tests for ...

(describe "Testing Example"
  (it "- t is equal t"
    (message "%s" org-files-db--install-directory)
    (expect t :to-be t)))
