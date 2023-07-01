;;; spec-mode.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Alexander Bakst
;;
;; Author: Alexander Bakst <abakst@certora.com>
;; Maintainer: Alexander Bakst <abakst@certora.com>
;; Created: June 28, 2023
;; Modified: June 28, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/alexanderbakst/spec-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary: Blorg.
;;
;;
;;; Code:
;;;
(require 'generic-x)

(defconst spec-keywords
  '("state" "init" "step" "when" "req"))

(defconst spec-types
  '("Int" "Array"))

(defconst spec-keywords-regexp
  (concat (rx symbol-start)
          (regexp-opt spec-keywords 'words)
          (rx symbol-end)))

(defconst spec-type-regexp
  (concat (rx symbol-start)
          (regexp-opt spec-types 'words)
          (rx symbol-end)))

(defconst spec-font-lock-keywords
  `((,spec-type-regexp . font-lock-type-face)
    ("step \\([a-z][A-Z_a-z]*\\)(" . '(1 font-lock-function-name-face))
    ("req \\([a-z][A-Z_a-z]*\\)" . '(1 font-lock-function-name-face))
    (,spec-keywords-regexp . font-lock-keyword-face)))

(defgroup specs nil
  "Major mode for spec source files."
  :group 'languages)


(defvar spec-syntax-table
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?# "< b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)
    synTable)
  "Syntax table for `spec-mode'.")

(define-derived-mode spec-mode text-mode "Specs"
  "Major mode for editing spec files."
  (make-local-variable 'spec-indent-offset)
  ;; (set (make-local-variable 'indent-line-function) 'spec-indent-line)
  (set (make-local-variable 'comment-start) "#")
  (set-syntax-table spec-syntax-table)
  (setq font-lock-defaults '((spec-font-lock-keywords))))

(provide 'spec-mode)
;;; spec-mode.el ends here
