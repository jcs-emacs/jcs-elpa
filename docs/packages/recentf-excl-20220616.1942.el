;;; recentf-excl.el --- Exclude commands for recent files  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-05-23 19:01:51

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/recentf-excl
;; Package-Version: 20220616.1942
;; Package-Commit: 8ee7e0c958e5efb41dc5ce3a53eb55fe8e891005
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: excl exclude recentf

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
;; Exclude commands for recent files
;;

;;; Code:

(defgroup recentf-excl nil
  "Exclude commands for recent files."
  :prefix "recentf-excl-"
  :group 'convenience
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/recentf-excl"))

(defcustom recentf-excl-commands nil
  "List of commands to ignore recent files."
  :type 'list
  :group 'recentf-excl)

;;;###autoload
(defvar recentf-excl-tracking-p t
  "If non-nil, track the opened file.")

;;;###autoload
(defmacro recentf-excl-it (&rest body)
  "Execute BODY and ignore recent files."
  (declare (indent 0) (debug t))
  `(let (recentf-excl-tracking-p) ,@body))

(defun recentf-excl--adv-around (fnc &rest args)
  "Advice bind FNC and ARGS."
  (recentf-excl-it (apply fnc args)))

(defun recentf-excl-mode--enable ()
  "Enable function `recentf-excl-mode'."
  (dolist (command recentf-excl-commands)
    (advice-add command :around #'recentf-excl--adv-around)))

(defun recentf-excl-mode--disable ()
  "Disable function `recentf-excl-mode'."
  (dolist (command recentf-excl-commands)
    (advice-remove command #'recentf-excl--adv-around)))

;;;###autoload
(define-minor-mode recentf-excl-mode
  "Minor mode 'recentf-excl-mode'."
  :global t
  :require 'recentf-excl-mode
  :group 'recentf-excl
  (if recentf-excl-mode (recentf-excl-mode--enable) (recentf-excl-mode--disable)))

(provide 'recentf-excl)
;;; recentf-excl.el ends here
