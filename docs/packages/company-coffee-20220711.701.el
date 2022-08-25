;;; company-coffee.el --- Emacs coffee completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2016, Noah Peart
;; Copyright (C) 2022, Jen-Chieh Shen

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Jen-Chieh Shen <jcs090218@gmail.com>
;; URL: https://github.com/elp-revive/company-coffee
;; Package-Version: 20220711.701
;; Package-Commit: cab6ebe0c1048881e565de98c3d159a05652db75
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (company "0.8.12"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Emacs autocompletion backend for coffee scripts using `company-mode'.
;; Just adding the keywords for now.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'company)

(defgroup company-coffee nil
  "Company completion backend for ."
  :group 'company
  :prefix "company-coffee-")

(defcustom company-coffee-modes '(coffee-mode)
  "Modes to activate `company-coffee'."
  :group 'company-coffee
  :type 'sexp)

;; Internal
(defvar company-keywords-alist)

(defvar company-coffee-compares
  '(("is" . "===")
    ("isnt" . "!==")
    ("not" . "!")
    ("and" . "&&")
    ("or" . "||")
    ("or=" . "||= (false, \"\", 0, null)")
    ("?=" . "||= (only when null/undefined)")
    ("true" . "true")
    ("yes" . "true")
    ("on" . "true")
    ("false" . "false")
    ("no" . "false")
    ("off" . "false")))

(defvar company-coffee-math
  '(("**" . "Math.pow")
    ("//" . "Math.floor(a, b)")
    ("%%" . "%")))

;; "var" "function"
(defvar company-coffee-words
  '("__extends" "__hasProp" "alert" "await" "break" "by" "case" "catch"
    "class" "const" "continue" "debugger" "default" "defer" "delete" "do"
    "else" "enum" "export" "extends" "finally" "for" "if"
    "import" "in" "instanceof" "let" "loop" "native" "new"
    "of" "or" "own" "return" "super" "switch" "then"
    "throw" "try" "typeof" "unless" "until" "void" "when" "while"
    "with" "yield" "undefined" "null"
    ))

(defvar company-coffee-keywords
  (sort
   (append
    (cl-loop for (k . v) in company-coffee-compares
             do (put-text-property 0 1 'annot v k)
             collect k)
    (cl-loop for (k . v) in company-coffee-math
             do (put-text-property 0 1 'annot v k)
             collect k)
    company-coffee-words)
   'string<))

(defun company-coffee-prefix ()
  (and (derived-mode-p major-mode company-coffee-modes)
       (not (company-in-string-or-comment))
       (company-grab-symbol)))

(defun company-coffee-candidates (arg)
  (all-completions arg company-coffee-keywords))

(defun company-coffee-annotation (candidate)
  (or (get-text-property 0 'annot candidate) ""))

;;;###autoload
(defun company-coffee (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-coffee))
    (prefix (company-coffee-prefix))
    (annotation (company-coffee-annotation arg))
    (sorted t)
    (candidates (company-coffee-candidates arg))))

(provide 'company-coffee)
;;; company-coffee.el ends here
