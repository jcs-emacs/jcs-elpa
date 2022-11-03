;;; helafy.el --- Minify/Uglify/Prettify contents  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/helafy
;; Package-Version: 20221103.1412
;; Package-Commit: ecab964cc9adf70e9475f63e3534216a92c0d6d9
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
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
;; Minify/Uglify/Prettify contents
;;

;;; Code:

(require 'json)
(require 'sgml-mode)

(defgroup helafy nil
  "Minify/Uglify/Prettify contents."
  :prefix "helafy-"
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/helafy"))

(defcustom helafy-pretty-functions
  `((json-mode . json-pretty-print)
    (nxml-mode . sgml-pretty-print)
    (xml-mode  . sgml-pretty-print)
    (web-mode  . sgml-pretty-print)
    (html-mode . sgml-pretty-print))
  "Alist mapping major modes to functions."
  :type '(alist :key-type symbol
                :value-type function)
  :group 'helafy)

(defcustom helafy-ugly-functions
  `((json-mode . (lambda (start end) (json-pretty-print start end t))))
  "Alist mapping major modes to functions."
  :type '(alist :key-type symbol
                :value-type function)
  :group 'helafy)

(defcustom helafy-default-ugly-function #'helafy-ugly-default
  "Alist mapping major modes to functions."
  :type 'functions
  :group 'helafy)

(defun helafy--region-bound ()
  "Return region boundary, else default to min/max."
  (if (use-region-p) (cons (region-beginning) (region-end))
    (cons (point-min) (point-max))))

;;;###autoload
(defun helafy-pretty ()
  "Prettify contents by file type."
  (interactive)
  (let* ((inhibit-modification-hooks t)
         (bound (helafy--region-bound))
         (start (car bound)) (end (cdr bound)))
    (if-let ((fnc (cdr (assq major-mode helafy-pretty-functions))))
        (funcall fnc start end)
      (user-error "[WARNING] No prettify command in this context"))))

(defun helafy-ugly-default (start end)
  "Default ugly function from START to END."
  (delete-whitespace-rectangle start end)
  (goto-char start)
  (while (search-forward "\n" nil t) (replace-match "" nil t)))

;;;###autoload
(defun helafy-ugly ()
  "Minify contents by removing newlines and whitespaces."
  (interactive)
  (let* ((inhibit-modification-hooks t)
         (bound (helafy--region-bound))
         (start (car bound)) (end (cdr bound)))
    (if-let ((fnc (or (cdr (assq major-mode helafy-ugly-functions))
                      helafy-default-ugly-function)))
        (funcall fnc start end)
      (user-error "[WARNING] No ugly command in this context"))))

(provide 'helafy)
;;; helafy.el ends here
