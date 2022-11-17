;;; build-packages.el --- Build package archive  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./scripts/_prepare.el")

(defconst per-section (string-to-number (getenv "PER_SECTION"))
  "Build this many packages in one section.")

(defconst elpa-section (string-to-number (getenv "ELPA_SECTION"))
  "Section ID.")

(defun recipe-in-section-p (count total)
  "Return t only when the recipe is in the right section."
  (let ((min (1+ (* per-section (1- elpa-section))))
        (max (* per-section elpa-section)))
    (and (<= min count) (<= count max))))

(with-package-build-env
  (make-directory package-build-archive-dir t)
  (let* ((recipes (directory-files package-build-recipes-dir nil "^[^.]"))
         (total (ceiling (/ (float (length recipes)) (float per-section))))
         (count 0))
    (message "BUILD SECTION: %s" elpa-section)
    (dolist (recipe recipes)
      (cl-incf count)
      (when (recipe-in-section-p count total)
        (message "")
        (message "")
        (message ":: github-elpa: packaging recipe %s" recipe)
        (let ((package-build-tar-executable (or github-elpa-tar-executable
                                                package-build-tar-executable)))
          ;; Currently no way to detect build failure...
          (ignore-errors (package-build-archive recipe)))))
    (package-build-cleanup)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; build-packages.el ends here
