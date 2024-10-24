; -*- mode: emacs-lisp; lexical-binding: t -*-

(eval-when-compile
  (defvar sentence-end-double-space)
  (defvar checkdoc-verb-check-experimental-flag))

;; Domain Specific Language
;; https://emacs-eask.github.io/DSL/
(package "org-files-db"
         "0.1.0"
         "Store data from Org files in a SQLite database")
(website-url "https://github.com/hubisan/org-files-db")
(keywords "outlines")
(license "GPLv3")

(package-file "org-files-db.el")
(files "org-files-db.el")

;; https://emacs-eask.github.io/DSL/#-source-alias
(source "gnu")
(source "nongnu")
(source "melpa")

;; https://emacs-eask.github.io/DSL/#-depends-on-package-name-rest-recipe
(depends-on "emacs" "29.1")
(depends-on "org")

;; https://emacs-eask.github.io/DSL/#-development-rest-body
(development
 (depends-on "buttercup")
 (depends-on "with-simulated-input"))

;; Tell checkdoc not to demand two spaces after a period.
(setq sentence-end-double-space nil)

;; Dont' check docstring for correct verb use.
(setq checkdoc-verb-check-experimental-flag nil)
