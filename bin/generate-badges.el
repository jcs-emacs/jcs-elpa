;;; generate-badges.el --- Generate all SVG badges  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./bin/prepare.el")
(require 'url-handlers)

(defconst svg-url-format
  "https://img.shields.io/badge/jcs%%20elpa-%s-6B8E23.svg"
  "Format to generate badges.")

(defconst output-dir "./badges/"  ; CI
  "Where the badges store.")

(ignore-errors (delete-directory output-dir t))  ; clean up first

(jcs-print "mkdir!")
(make-directory output-dir)

(jcs-print "?")

(dolist (pkg archive-contents)
  (jcs-print pkg)
  (let* ((pkg-name (car pkg)) (desc (cdr pkg))
         (version (aref desc 0))
         (version (mapconcat (lambda (item) (format "%s" item)) version "."))
         (url (format svg-url-format version))
         (file (concat output-dir pkg-name ".svg")))
    (message "Download SVG from `%s` to `%s`" url file)
    (url-copy-file url file)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; generate-badges.el ends here
