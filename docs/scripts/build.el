;;; build.el --- Build archive json  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

(setq package-archives
      '(("jcs" . "https://jcs-emacs.github.io/elpa/elpa/")))

(setq package-enable-at-startup nil  ; To avoid initializing twice
      package-check-signature nil)

(package-refresh-contents)

(let (json)
  (dolist (pkg package-archive-contents)
    (let* ((pkg-name (car pkg)) (desc (car (cdr pkg)))
           (version (package-desc-version desc))
           (summary (package-desc-summary desc))
           (extras (package-desc-extras desc))
           (url (cdr (assq :url extras))))
      (push (cons "name" pkg-name) json)
      (push (cons "summary" summary) json)
      (push (cons "version" version) json)
      (push (cons "url" url) json)))
  (write-region (json-encode json) nil "../archive.json"))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; build.el ends here
