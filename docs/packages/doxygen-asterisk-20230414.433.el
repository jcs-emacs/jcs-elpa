;;; doxygen-asterisk.el --- Minor mode that helps you insert pair /* and */  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/doxygen-asterisk
;; Package-Version: 20230414.433
;; Package-Commit: e3cd32ab2fceb7ebaddc0f80925e4dca5cf1af35
;; Version: 0.1.0
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
;; Minor mode that helps you insert pair /* and */
;;

;;; Code:

(defgroup doxygen-asterisk nil
  "Minor mode that helps you insert pair /* and */."
  :prefix "doxygen-asterisk-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/doxygen-asterisk"))

;;
;; (@* "Entry" )
;;

(defun doxygen-asterisk-mode--enable ()
  "Enable `doxygen-asterisk' in current buffer."
  (advice-add (key-binding (kbd "*")) :around #'doxygen-asterisk--key))

;;;###autoload
(define-minor-mode doxygen-asterisk-mode
  "Minor mode `doxygen-asterisk-mode'."
  :lighter " Doxy*"
  :group doxygen-asterisk
  (when doxygen-asterisk-mode (doxygen-asterisk-mode--enable)))

;;
;; (@* "Util" )
;;

(defun doxygen-asterisk--last-char-in-line-p (&optional pt)
  "Return non-nil if there is nothing behind of the right from the PT."
  (save-excursion
    (when pt (goto-char pt))
    (null (re-search-forward "[^ \t]" (line-end-position) t))))

(defun doxygen-asterisk--looking-back (regexp &optional limit greedy)
  "Wrapper for function `looking-back'.
See function `looking-back' description for arguments REGEXP, LIMIT,
and GREEDY."
  (ignore-errors (looking-back regexp limit greedy)))

;;
;; (@* "Key" )
;;

(defcustom doxygen-asterisk-modes
  '( c-mode c++-mode objc-mode csharp-mode swift-mode
     jai-mode
     java-mode groovy-mode processing-mode
     javascript-mode js-mode js2-mode js3-mode json-mode rjsx-mode
     web-mode php-mode
     actionscript-mode typescript-mode
     go-mode scala-mode
     rust-mode rustic-mode
     css-mode ssass-mode scss-mode
     shader-mode)
  "List of `major-mode' that can be use Doxygen style."
  :type 'list
  :group 'doxygen-asterisk)

(defun doxygen-asterisk--like-p ()
  "Return non-nil if current `major-mode' use Doxygen style."
  (memq major-mode doxygen-asterisk-modes))

(defun doxygen-asterisk--key (fnc &rest args)
  "Asterisk key for Doxygen like document string.

This fulfill condition, /* with */ into a pair."
  (apply fnc args)
  (when (and doxygen-asterisk-mode (doxygen-asterisk--like-p))
    (save-excursion
      (when (and (doxygen-asterisk--last-char-in-line-p)
                 (doxygen-asterisk--looking-back "/[*]" 2))
        (insert "*/")))))

(provide 'doxygen-asterisk)
;;; doxygen-asterisk.el ends here
