;;; build.el --- Build package archive  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'github-elpa)

(setq github-elpa-working-dir "./.github-elpa-working"
      github-elpa-archive-dir "./docs/packages"
      github-elpa-recipes-dir "./recipes")

(github-elpa-build)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; build.el ends here
