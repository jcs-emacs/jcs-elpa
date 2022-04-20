;;; mbs.el --- Minibuffer Stats  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-04-19 21:01:22

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Minibuffer Stats
;; Keyword: minibuffer stats
;; Version: 0.1.0
;; Package-Version: 20220419.1307
;; Package-Commit: c73464443ee4352749174b33bfb312ce1c271613
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/jcs-elpa/mbs

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
;; Minibuffer Stats
;;

;;; Code:

(defun mbs-do-stuff (fnc &rest args)
  "Execute FNC and ARGS in minibuffer the safe way."
  (if (not (active-minibuffer-window))
      (user-error "[ERROR] Minibuffer not active to do stuff: %s" fnc)
    (save-selected-window
      (select-window (active-minibuffer-window))
      (apply fnc args))))

(defun mbs--compare-p (name type)
  "Compare `buffer-string' with NAME and TYPE."
  (mbs-do-stuff (lambda () (jcs-string-compare-p name (buffer-string) type))))

;;;###autoload
(defun mbs-M-x-p ()
  "Return non-nil if current minibuffer M-x."
  (mbs--compare-p "M-x" 'regex))

;;;###autoload
(defun mbs-finding-file-p ()
  "Return non-nil if current minibuffer finding file."
  (mbs--compare-p "Find file" 'regex))

;;;###autoload
(defun mbs-renaming-p ()
  "Return non-nil if current minibuffer renaming."
  (mbs--compare-p "New name:" 'regex))

(provide 'mbs)
;;; mbs.el ends here
