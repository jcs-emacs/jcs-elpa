;;; eask-mode.el --- major mode for editing Eask files  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-03-14 03:38:51

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-eask/eask-mode
;; Package-Version: 20221226.504
;; Package-Commit: 1a19f08c8f11981bbd9fac74bae1ce2dba871f9c
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (eask "0.1.0"))
;; Keywords: lisp eask

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; major mode for editing Eask files.
;;

;;; Code:

(require 'eask-core)

(defvar eask-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?: "_" table)
    table))

(defface eask-mode-symbol-face
  '((t :inherit font-lock-constant-face))
  "Face for highlighting symbols (e.g. :git) in Eask files."
  :group 'eask-mode)

(defconst eask-mode-font-lock-keywords
  `((,(regexp-opt
       eask-file-keywords
       'symbols)
     . font-lock-keyword-face)
    (,(rx symbol-start
          (or ":github" ":gitlab" "bitbucket" "wiki"
              ":git" ":bzr" ":hg" ":darcs" ":fossil" ":svn" ":cvs")
          symbol-end)
     . eask-mode-symbol-face))
  "Keywords in Eask file.")

;;;###autoload
(define-derived-mode eask-mode emacs-lisp-mode "Eask"
  "Major mode for editing Eask files."
  :syntax-table eask-mode-syntax-table
  (font-lock-add-keywords 'eask-mode eask-mode-font-lock-keywords)
  ;; FIXME: toggling comments only applies to the current line,
  ;; breaking multiline sexps.
  (setq-local comment-start ";; ")
  (setq-local comment-end "")
  (setq-local indent-line-function #'lisp-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("/Easkfile[.0-9]*\\'" . eask-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("/Eask[.0-9]*\\'" . eask-mode))

(provide 'eask-mode)
;;; eask-mode.el ends here
