;;; company-kaomoji.el --- Company backend for Kaomoj  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/company-kaomoji
;; Package-Version: 20220727.1816
;; Package-Commit: 0c4b26fb19de00ac3fcbb63673d4a0faa9c9b9f0
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (company "0.8.12") (kaomoji "0.1.0") (ht "2.0"))
;; Keywords: matching kaomoji

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
;; Company backend for Kaomoj
;;

;;; Code:

(require 'company)
(require 'kaomoji)
(require 'ht)

(defgroup company-kaomoji nil
  "Company backend for Kaomoj."
  :prefix "company-kaomoji-"
  :group 'company
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/company-kaomoji"))

(defcustom company-kaomoji-annotation t
  "If non-nil, display kaomoji with annotation."
  :type 'boolean
  :group 'company-kaomoji)

(defcustom company-kaomoji-document t
  "If non-nil, display kaomoji information."
  :type 'boolean
  :group 'company-kaomoji)

(defvar-local company-kaomoji--data nil
  "Data is used for uniquify candidates.")

(defun company-kaomoji--prefix ()
  "Return prefix."
  (or (company-grab-line) 'stop))

(defun company-kaomoji--add-shortcode (shortcode kaomoji &optional level new-name)
  "Add KAOMOJI with uniquify SHORTCODE.

Optional argument is use to uniquify."
  (setq level (or level 1))
  ;; uniquify the name
  (if (ht-get company-kaomoji--data (or new-name shortcode))
      (company-kaomoji--add-shortcode shortcode kaomoji (1+ level)
                                      (format "%s %s" shortcode level))
    (ht-set company-kaomoji--data (or new-name shortcode) kaomoji)))

(defun company-kaomoji--candidates (prefix)
  "Return candidates with PREFIX."
  (setq company-kaomoji--data (ht-create))
  (dolist (data (kaomoji-internal-get-candidates prefix))
    (let ((shortcode (car data)) (kaomoji (cdr data)))
      (company-kaomoji--add-shortcode shortcode kaomoji)))
  (ht-keys company-kaomoji--data))

(defun company-kaomoji--annotation (candidate)
  "Return annotation for CANDIDATE."
  (when (and company-kaomoji-annotation company-kaomoji--data)
    (ht-get company-kaomoji--data candidate)))

(defun company-kaomoji--doc-buffer (candidate)
  "Return document for CANDIDATE."
  (company-doc-buffer
   (if (and company-kaomoji-document company-kaomoji--data)
       (ht-get company-kaomoji--data candidate)
     "")))

;;;###autoload
(defun company-kaomoji (command &optional arg &rest ignored)
  "Company backend for Kaomoj.

Arguments COMMAND, ARG and IGNORED are standard arguments from `company-mode`."
  (interactive (list 'interactive))
  (cl-case command
    (`interactive (company-begin-backend 'company-kaomoji))
    (`prefix (company-kaomoji--prefix))
    (`annotation (company-kaomoji--annotation arg))
    (`candidates (company-kaomoji--candidates arg))
    (`doc-buffer (company-kaomoji--doc-buffer arg))
    (`meta (company-kaomoji--annotation arg))
    (`post-completion
     (kill-region (- (point) (length arg)) (point))
     (when company-kaomoji--data
       (insert (ht-get company-kaomoji--data arg))))))

(provide 'company-kaomoji)
;;; company-kaomoji.el ends here

^;;; company-kaomoji.el ends here
