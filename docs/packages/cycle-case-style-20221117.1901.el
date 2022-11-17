;;; cycle-case-style.el --- Cycle through case style (PascalCase, camelCase, etc)  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/cycle-case-style
;; Package-Version: 20221117.1901
;; Package-Commit: f4a1d34ced3d4384ede85c998da9c8f1d074108d
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (change-case "9"))
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
;; Cycle through case style (PascalCase, camelCase, etc)
;;

;;; Code:

(require 'cl-lib)

(require 'change-case)

(defgroup cycle-case-style nil
  "Cycle through case style (PascalCase, camelCase, etc)."
  :prefix "cycle-case-style-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/cycle-case-style"))

(defun cycle-case-style--next-renderer (parser)
  "Get next case renderer by PARSER."
  (let* ((result (cl-member parser
                            change-case-renderer-alist
                            :test
                            (lambda (parser renderer)
                              (equal (car parser) (car renderer)))))
         (len (length result)))
    (cond ((zerop len) (user-error "Invalid parser for `cycle-case-style': %s" parser))
          ((= len 1) (nth 0 change-case-renderer-alist))
          (t (nth 1 result)))))

(defun cycle-case-style--guess-parser (sentence)
  "Guess the case parser from SENTENCE."
  (cl-some (lambda (parser)
             (unless (= 1 (length (funcall (cdr parser) sentence)))
               (pcase (car parser)
                 ("PascalCase"
                  ;; XXX: It seems like there is an issue in PascalCase parser,
                  ;; let's do it dirty and manually give camelCase when the
                  ;; first character is the lowercase letter.
                  (if-let* ((char (substring sentence 0 1))
                            ((string= char (downcase char))))
                      (assq "camelCase" change-case-parser-alist)
                    parser))
                 (_ parser))))
           change-case-parser-alist))

;;;###autoload
(defun cycle-case-style (&optional start end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (if-let (bounds (bounds-of-thing-at-point 'symbol))
                     (list (car bounds) (cdr bounds))
                   (list (point-min) (point-max)))))
  (let* ((sentence (buffer-substring-no-properties start end))
         (parser (cycle-case-style--guess-parser sentence))
         (renderer (cycle-case-style--next-renderer parser)))
    (change-case-edit
     start end
     (funcall (cdr renderer) (funcall (cdr parser) sentence)))))

(provide 'cycle-case-style)
;;; cycle-case-style.el ends here
