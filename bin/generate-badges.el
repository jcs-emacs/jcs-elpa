;;; generate-badges.el --- Generate all SVG badges  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./prepare.el")

(require 'url-handlers)

(defconst svg-url-format
  "https://img.shields.io/badge/jcs%%20elpa-%s-6B8E23.svg"
  "Format to generate badges.")

(defconst output-dir "./badges/"  ; CI
  "Where the badges store.")

(ignore-errors (delete-directory output-dir t))  ; clean up first
(make-directory output-dir)



;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; generate-badges.el ends here
