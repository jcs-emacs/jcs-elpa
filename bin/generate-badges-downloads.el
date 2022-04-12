;;; generate-badges-downloads.el --- Generate SVG badges, version  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./bin/_prepare.el")
(require 'url-handlers)

(defconst svg-url-format
  "https://img.shields.io/badge/downloads-%s-6B8E23.svg"
  "Format to generate badges.")

(defconst output-dir "./badges/d/"
  "Where the badges store.")

(ignore-errors (delete-directory output-dir t))  ; clean up first
(make-directory output-dir t)                    ; recursive

;; TODO: ..

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; generate-badges-downloads.el ends here
