;;; build.el --- Build archive json  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

(setq package-archives
      '(("jcs" . "https://jcs-emacs.github.io/elpa/elpa/")))

(setq package-enable-at-startup nil  ; To avoid initializing twice
      package-check-signature nil)

(package-refresh-contents)

(message "%s" package-archive-contents)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; build.el ends here
