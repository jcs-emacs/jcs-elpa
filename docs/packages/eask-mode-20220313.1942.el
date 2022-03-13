;;; eask-mode.el --- major mode for editing Eask files  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-03-14 03:38:51

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: major mode for editing Eask files.
;; Keyword: eask
;; Version: 0.1.0
;; Package-Version: 20220313.1942
;; Package-Commit: d6ba02b5dcadefeedc5fad3135e3a971531e5618
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/emacs-eask/eask-mode

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
;; major mode for editing Cask files.
;;

;;; Code:

(defvar eask-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?: "_" table)
    table))

;;;###autoload
(define-derived-mode eask-mode emacs-lisp-mode "Eask"
  "Major mode for editing Eask files."
  :syntax-table eask-mode-syntax-table
  ;; FIXME: toggling comments only applies to the current line,
  ;; breaking multiline sexps.
  (setq-local comment-start ";; ")
  (setq-local comment-end "")
  (setq-local indent-line-function #'lisp-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("/Eask\\'" . eask-mode))

(provide 'eask-mode)
;;; eask-mode.el ends here
