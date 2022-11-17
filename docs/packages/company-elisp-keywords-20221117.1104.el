;;; company-elisp-keywords.el --- Company completion for `finder-known-keywords'  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/company-elisp-keywords
;; Package-Version: 20221117.1104
;; Package-Commit: 2a6b96beb116202b3e7fd3d455d83bda7eaf1520
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (company "0.8.0"))
;; Keywords: lisp

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
;; Company completion for `finder-known-keywords'
;;

;;; Code:

(require 'cl-lib)
(require 'finder)

(require 'company)

(defgroup company-elisp-keywords nil
  "Company completion for `finder-known-keywords'."
  :prefix "company-elisp-keywords-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/company-elisp-keywords"))

(defvar company-elisp-keywords--candidates nil
  "Cache for candidates.")

;;
;; (@* "Util" )
;;

(defun company-elisp-keywords--2-str (object)
  "Convert OBJECT to string."
  (format "%s" object))

;;
;; (@* "Core" )
;;

(defun company-elisp-keywords--prepare ()
  "Prepare data."
  (unless company-elisp-keywords--candidates
    (setq company-elisp-keywords--candidates (mapcar (lambda (keyword)
                                                       (company-elisp-keywords--2-str (car keyword)))
                                                     finder-known-keywords))))

(defun company-elisp-keywords--doc-buffer (candidate)
  "Return document for CANDIDATE."
  (company-doc-buffer
   (company-elisp-keywords--2-str (cdr (assq (intern candidate) finder-known-keywords)))))

;;;###autoload
(defun company-elisp-keywords (command &optional arg &rest ignored)
  "Company backend for `finder-known-keywords'.

Arguments COMMAND, ARG and IGNORED are standard arguments from `company-mode`."
  (interactive (list 'interactive))
  (company-elisp-keywords--prepare)
  (cl-case command
    (`interactive (company-begin-backend 'company-elisp-keywords))
    (`prefix (and (derived-mode-p 'emacs-lisp-mode)
                  (string-match-p "^;[; ]+Keywords:" (thing-at-point 'line))
                  (company-grab-symbol)))
    (`candidates company-elisp-keywords--candidates)
    (`doc-buffer (company-elisp-keywords--doc-buffer arg))))

(provide 'company-elisp-keywords)
;;; company-elisp-keywords.el ends here
