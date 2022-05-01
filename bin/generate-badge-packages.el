;;; generate-badge-packages.el --- Generate SVG badge, packages  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./bin/_prepare.el")
(require 'url-handlers)

(defconst svg-url-format
  "https://img.shields.io/badge/Packages-%s-6B8E23.svg?logo=hack-the-box"
  "Format to generate badges.")

(defconst output-dir "./badges/"
  "Where the badges store.")

(ignore-errors (make-directory output-dir t))

;; Packages
(let* ((file (concat output-dir "packages.svg"))
       (url (format svg-url-format (length archive-contents))))
  (message "Downloading SVG from `%s` to `%s`... done!" url file)
  (url-copy-file url file t))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; generate-badge-packages.el ends here
