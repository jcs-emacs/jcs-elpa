;;; mbs.el --- Minibuffer Stats  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-04-19 21:01:22

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/mbs
;; Package-Version: 20221221.1907
;; Package-Commit: d7a0a8b52eb2d4e8bb53bd853535f588120fe2cb
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience minibuffer stats

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

(defmacro mbs--with-minibuffer-env (&rest body)
  "Execute BODY with minibuffer variables."
  (declare (indent 0) (debug t))
  `(let ((prompt (minibuffer-prompt))
         (contents (minibuffer-contents)))
     ,@body))

;;;###autoload
(defun mbs-M-x-p ()
  "Return non-nil if current minibuffer `M-x'."
  (mbs--with-minibuffer-env
    (string-prefix-p "M-x" prompt)))

;;;###autoload
(defun mbs-finding-file-p ()
  "Return non-nil if current minibuffer finding file."
  (mbs--with-minibuffer-env
    (and (not (mbs-M-x-p))
         (not (string-empty-p contents))
         (ignore-errors (expand-file-name contents)))))

;;;###autoload
(defun mbs-renaming-p ()
  "Return non-nil if current minibuffer renaming."
  (mbs--with-minibuffer-env
    (string-prefix-p "New name:" prompt)))

(provide 'mbs)
;;; mbs.el ends here
