;;; build.el --- Build package archive  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'github-elpa)

(setq github-elpa-working-dir "./.github-elpa-working"
      github-elpa-archive-dir "./docs/packages"
      github-elpa-recipes-dir "./recipes")

(let ((package-build-working-dir (expand-file-name github-elpa-working-dir))
      (package-build-archive-dir (expand-file-name github-elpa-archive-dir))
      (package-build-recipes-dir (expand-file-name github-elpa-recipes-dir)))
  ;;(github-elpa--git-check-repo)
  ;;(github-elpa--git-check-workdir-clean)
  (make-directory package-build-archive-dir t)
  ;; Currently no way to detect build failure...
  (dolist (recipe (directory-files package-build-recipes-dir nil "^[^.]"))
    (message "")
    (message "")
    (message ":: github-elpa: packaging recipe %s" recipe)
    (let ((package-build-tar-executable (or github-elpa-tar-executable
                                            package-build-tar-executable)))
      (package-build-archive recipe)))
  (package-build-cleanup))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; build.el ends here
