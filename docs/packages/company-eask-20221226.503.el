;;; company-eask.el --- Company backend for Eask-file  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-eask/company-eask
;; Package-Version: 20221226.503
;; Package-Commit: 1ede53add8aa7795325d7bb1f35139bb6fb5f196
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (company "0.8.0") (eask "0.1.0"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Company backend for Eask-file
;;

;;; Code:

(require 'cl-lib)

(require 'company)
(require 'company-elisp)
(require 'eask-core)

(defgroup company-eask nil
  "Company completion for Eask-file."
  :prefix "company-eask-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-eask/company-eask"))

;;
;; (@* "Core" )
;;

(defun company-eask--s-replace (old new s)
  "String replace."
  (if (fboundp #'string-replace)
      (string-replace old new s)
    (replace-regexp-in-string (regexp-quote old) new s t t)))

(defun company-eask--improve-doc (symbol)
  "Display only the directive name, and replace alias description."
  (let* ((buf-str (with-current-buffer (help-buffer) (buffer-string)))
         (str (company-eask--s-replace "eask-f-" "" buf-str))
         (str (company-eask--s-replace
               " is a Lisp closure "
               (format " is an alias for ‘%s’ "
                       (propertize (eask-2str symbol) 'face
                                   `( :foreground "cyan"
                                      :underline t)))
               str)))
    str))

(defun company-eask--candidates ()
  "Return a list of candidates."
  eask-file-keywords)

(defun company-eask--annotation (_candidate)
  "Return annotation for CANDIDATE."
  "(Directive)")

(defun company-eask--doc-buffer (candidate)
  "Return document for CANDIDATE."
  (let ((symbol (intern (format "eask-f-%s" candidate))))
    (save-window-excursion
      (ignore-errors
        (cond
         ((fboundp symbol) (describe-function symbol))
         (t (signal 'user-error nil)))
        (company-doc-buffer (company-eask--improve-doc symbol))))))

;;
;; (@* "Entry" )
;;

;;;###autoload
(defun company-eask (command &optional arg &rest ignored)
  "Company backend for Eask-file.

Arguments COMMAND, ARG and IGNORED are standard arguments from `company-mode`."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eask))
    (prefix (and (derived-mode-p 'eask-mode)
                 (company-elisp--prefix)))
    (candidates (company-eask--candidates))
    (annotation (company-eask--annotation arg))
    (doc-buffer (company-eask--doc-buffer arg))))

(provide 'company-eask)
;;; company-eask.el ends here
