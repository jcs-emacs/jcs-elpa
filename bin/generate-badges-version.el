;;; generate-badges-version.el --- Generate SVG badges, version  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./bin/_prepare.el")
(require 'url-handlers)

(defconst svg-url-format
  "https://img.shields.io/badge/jcs%%20elpa-%s-6B8E23.svg"
  "Format to generate badges.")

(defconst output-dir "./badges/v/"
  "Where the badges store.")

(ignore-errors (delete-directory output-dir t))  ; clean up first
(make-directory output-dir t)                    ; recursive

(dolist (pkg archive-contents)
  (message "%s" pkg)
  (let* ((pkg-name (format "%s" (car pkg))) (desc (cdr pkg))
         (version (package-version-join (aref desc 0)))
         (url (format svg-url-format version))
         (file (concat output-dir pkg-name ".svg")))
    (message "Download SVG from `%s` to `%s`" url file)
    (url-copy-file url file t)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; generate-badges-version.el ends here
