;;; cycle-slash.el --- Cycle through slash, backslash, and double backslash  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/cycle-slash
;; Package-Version: 20221117.1154
;; Package-Commit: 839218d28c609ce1ff6ff2f84eb2edfd6f19e1f6
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
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
;; Cycle through slash, backslash, and double backslash
;;

;;; Code:

(require 'cl-lib)

(defgroup cycle-slash nil
  "Cycle through slash, backslash, and double backslash."
  :prefix "cycle-slash-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/cycle-slash"))

(defcustom cycle-slash-chars
  '("/" "\\" "\\\\")
  "A list of slash character to cycle."
  :type 'list
  :group 'cycle-slash)

(defun cycle-slash--next-char (char)
  "Get next slash chararcter by CHAR in cycle."
  (let* ((result (member char cycle-slash-chars))
         (len (length result)))
    (cond ((zerop len) (user-error "Invalid character for `cycle-slash': %s" char))
          ((= len 1) (nth 0 cycle-slash-chars))
          (t (nth 1 result)))))

(defun cycle-slash--find-slash (str)
  "Return the first occurrence slash character in STR."
  (cl-some (lambda (char)
             (and (string-match-p (regexp-quote char) str)
                  char))
           (sort cycle-slash-chars
                 ;; Find slash with higher length first!
                 (lambda (str1 str2) (> (length str1) (length str2))))))

(defun cycle-slash--string (str)
  "Apply cycle slash to STR."
  (let ((char (cycle-slash--find-slash str)))
    (string-replace char (cycle-slash--next-char char) str)))

;;;###autoload
(defun cycle-slash ()
  "Cycle slash."
  (interactive)
  (when-let ((str (thing-at-point 'string))
             (bound (bounds-of-thing-at-point 'string))
             (orig-point (point)))
    (delete-region (car bound) (cdr bound))
    (insert (cycle-slash--string str))
    (goto-char orig-point)))

(provide 'cycle-slash)
;;; cycle-slash.el ends here
