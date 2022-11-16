;;; _prepare.el --- Prepration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(require 'thingatpt)
(require 'json)
(require 'subr-x)

(defconst archive-contents-string
  (with-temp-buffer
    (or (ignore-errors (insert-file-contents "../docs/packages/archive-contents"))
        (ignore-errors (insert-file-contents "./docs/packages/archive-contents")))  ; CI
    (buffer-string))
  "Archive content string.")

(defconst archive-contents
  (eval (thing-at-point--read-from-whole-string
         (concat "'" archive-contents-string)))
  "Turn it into lisp object.")

(pop archive-contents)  ; remove 1

;;
;;; Util

(defun file-to-string (file)
  "File to string function."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

;;
;;; Build

(require 'github-elpa)

(setq github-elpa-working-dir "./.github-elpa-working"
      github-elpa-archive-dir "./docs/packages"
      github-elpa-recipes-dir "./recipes")

(defmacro with-package-build-env (&rest body)
  "Set up package build environment then execute the BODY form."
  (declare (indent 0))
  `(let ((package-build-working-dir (expand-file-name github-elpa-working-dir))
         (package-build-archive-dir (expand-file-name github-elpa-archive-dir))
         (package-build-recipes-dir (expand-file-name github-elpa-recipes-dir)))
     ,@body))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; _prepare.el ends here
