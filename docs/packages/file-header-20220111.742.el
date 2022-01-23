;;; file-header.el --- Highly customizable self design file header  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022  Shen, Jen-Chieh
;; Created date 2018-12-24 16:49:42

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Highly customizable self design file header.
;; Keyword: file header
;; Version: 0.1.2
;; Package-Version: 20220111.742
;; Package-Commit: ad3b50667c8e9bbc2ef69a2ba5ca485b7a19319f
;; Package-Requires: ((emacs "24.4"))
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
  (let ((tmp-ini (file-header--get-string-from-file path))
        (tmp-keyword "") (tmp-value "")
        (count 0) tmp-ini-list tmp-pair-list)
    (setq tmp-ini (split-string tmp-ini "\n"))

    (dolist (tmp-line tmp-ini)
      ;; check not comment
      (when (not (string-match-p "#" tmp-line))
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
      (setq count (1+ count)))

    ;; return list.
    tmp-ini-list))

;;;###autoload
(defun file-header-swap-keyword-template (template-str)
  "Swap all keyword in TEMPLATE-STR to proper information."
  (let ((tmp-keyword "") (tmp-value "") (tmp-index 0) tmp-ini-list)
    ;; parse and get the list of keyword and value.
    (setq tmp-ini-list (file-header--parse-ini file-header-template-config-filepath))

    (while (< tmp-index (length tmp-ini-list))
      (setq tmp-keyword (nth tmp-index tmp-ini-list)
            tmp-value (nth (1+ tmp-index) tmp-ini-list))

      ;; Add `#' infront and behind the keyword.
      ;; For instance, `CREATOR' -> `#CREATOR#'.
      (setq tmp-keyword (concat "#" tmp-keyword)
            tmp-keyword (concat tmp-keyword "#"))

      ;; NOTE: Check keyword exist before replacing it.
      ;; Or else it will cause `max-lisp-eval-depth' error.
      (when (string-match-p tmp-keyword template-str)
        ;; Check if the value is a function?
        (if (string-match-p "(" tmp-value)
            (progn
              ;; Remove `(' and `)', if is a function.
              (setq tmp-value (file-header--s-replace "(" "" tmp-value)
                    tmp-value (file-header--s-replace ")" "" tmp-value)
                    template-str (file-header--s-replace tmp-keyword
                                                         (funcall (intern tmp-value))
                                                         template-str)))
          ;; Replace it normally with a string.
          (setq template-str (file-header--s-replace tmp-keyword
                                                     tmp-value
                                                     template-str))))
      ;; Add 2 to skip keyword and value at the same time.
      (setq tmp-index (+ tmp-index 2))))

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
