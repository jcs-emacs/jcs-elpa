;;; file-header.el --- Highly customizable self design file header  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022  Shen, Jen-Chieh
;; Created date 2018-12-24 16:49:42

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Highly customizable self design file header.
;; Keyword: file header
;; Version: 0.1.2
;; Package-Version: 20220213.1804
;; Package-Commit: ee70c2cc079d8da7d5cb7e4544d40c9d7f9ade3a
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/jcs-elpa/file-header

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Highly customizable self design file header.
;;

;;; Code:

(require 'thingatpt)

(defgroup file-header nil
  "Highly customizable self design file header."
  :prefix "file-header-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/file-header"))

(defcustom file-header-template-config-filepath ""
  "File path ot template config properties."
  :type 'string
  :group 'file-header)

(defun file-header--s-replace (old new s)
  "Replace OLD with NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun file-header--get-string-from-file (path)
  "Return PATH's file content."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun file-header--parse-ini (path)
  "Parse a .ini file from PATH."
  (let* ((tmp-ini (file-header--get-string-from-file path))
         (tmp-ini (split-string tmp-ini "\n"))
         (tmp-keyword "") (tmp-value "")
         (count 0) tmp-ini-list tmp-pair-list)

    (dolist (tmp-line tmp-ini)
      ;; check not comment
      (unless (string-match-p "#" tmp-line)
        ;; Split it
        (setq tmp-pair-list (split-string tmp-line "="))

        ;; Assign to temporary variables
        (setq tmp-keyword (nth 0 tmp-pair-list)
              tmp-value (nth 1 tmp-pair-list))

        ;; Check empty value
        (when (and (not (string= tmp-keyword "")) tmp-value)
          (let (tmp-list)
            (push tmp-keyword tmp-list)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))
          (let (tmp-list)
            (push tmp-value tmp-list)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))))
      (cl-incf count))

    ;; return list
    tmp-ini-list))

;;;###autoload
(defun file-header-swap-keyword-template (template-str)
  "Swap all keyword in TEMPLATE-STR to proper information."
  (let ((tmp-keyword "") (tmp-value "") (tmp-index 0) tmp-ini-list)
    ;; parse and get the list of keyword and value
    (setq tmp-ini-list (file-header--parse-ini file-header-template-config-filepath))

    (while (< tmp-index (length tmp-ini-list))
      (setq tmp-keyword (nth tmp-index tmp-ini-list)
            tmp-value (nth (1+ tmp-index) tmp-ini-list))

      ;; Add `#' infront and behind the keyword; for instance, `CREATOR' to `#CREATOR#'
      (setq tmp-keyword (format "#%s#" tmp-keyword))

      ;; NOTE: Check keyword exist before replacing it
      ;; Or else it will cause `max-lisp-eval-depth' error
      (when (string-match-p tmp-keyword template-str)
        ;; Check if the value is a snippet
        (if (string-match-p "`" tmp-value)
            ;; Remove ` for evaluation
            (setq tmp-value (file-header--s-replace "`" "" tmp-value)
                  template-str (file-header--s-replace tmp-keyword
                                                       (eval (thing-at-point--read-from-whole-string tmp-value))
                                                       template-str))
          ;; Replace it normally with a string
          (setq template-str (file-header--s-replace tmp-keyword
                                                     tmp-value
                                                     template-str))))
      ;; Add 2 to skip keyword and value at the same time
      (cl-incf tmp-index 2)))

  ;; return itself.
  template-str)

;;;###autoload
(defun file-header-get-template-by-file-path (path)
  "Swap all keywords then return it from the PATH."
  (file-header-swap-keyword-template (file-header--get-string-from-file path)))

;;;###autoload
(defun file-header-insert-template-by-file-path (path)
  "Swap all keywords from the PATH then insert it to current buffer."
  (insert (file-header-get-template-by-file-path path)))

(provide 'file-header)
;;; file-header.el ends here
