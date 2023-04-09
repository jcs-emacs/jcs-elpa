;;; generate-eld.el --- Generate elpa-packages.eld  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./scripts/_prepare.el")

(with-package-build-env
  (make-directory package-build-archive-dir t)
  (package-build-cleanup))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; generate-eld.el ends here
