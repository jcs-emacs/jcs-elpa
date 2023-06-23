;;; qsharp-mode.el --- Programming mode for the Q# language  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Siegmentation Fault <siegmentationfault@yandex.ru>
;; Maintainer: Siegmentation Fault <siegmentationfault@yandex.ru>
;; URL: https://github.com/forked-from-1kasper/emacs-qsharp-mode
;; Package-Version: 20230502.727
;; Package-Commit: 402bf231c823776480323c00a3dba85f9467e0cf
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: languages, q#, quantum

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
;; Add a Q# mode to Emacs.
;;

;;; Code:

(defconst qsharp-mode-syntax-table
  (let ((table (make-syntax-table)))
	(modify-syntax-entry ?\/ ". 12b" table)
	(modify-syntax-entry ?\n "> b" table)
	(modify-syntax-entry ?\" "\"" table)
	table))

(defun qsharp-regexp-keyword (name)
  `((concat "\\b" name "\\b") . font-lock-keyword-face))

(defun qsharp-make-big-keyword (names)
  (mapconcat
   #'(lambda (name) (concat "\\b" name "\\b")) names "\\|"))

(defun qsharp-regexps (some-lock names)
  `((,(qsharp-make-big-keyword names) . ,some-lock)))

(defconst qsharp-keywords '("set" "mutable" "let" "body" "operation" "using" "open" "namespace" "newtype" "adjoint" "auto" "controlled" "function" "new"))
(defconst qsharp-constants '("One" "Zero" "PauliX" "PauliY" "PauliZ" "PauliI" "true" "false"))
(defconst qsharp-builtins '("if" "elif" "else" "fail" "for" "in" "return" "repeat" "until" "fixup"))
(defconst qsharp-functions '("M" "H" "CNOT" "CCNOT" "I" "Measure" "Message" "Assert" "AssertProb" "Exp" "ExpFrac" "R" "R1" "RFrac" "R1Frac" "Random" "Reset" "ResetAll" "Rx" "Ry" "Rz" "S" "SWAP" "T" "X" "Y" "Z" "MultiX" "Length"))
(defconst qsharp-types '("Int" "Double" "String" "Bool" "Qubit" "Pauli" "Result" "Range"))

(defconst qsharp-highlightings
  (append (qsharp-regexps font-lock-keyword-face qsharp-keywords)
		  (qsharp-regexps font-lock-constant-face qsharp-constants)
		  (qsharp-regexps font-lock-builtin-face qsharp-builtins)
		  (qsharp-regexps font-lock-function-name-face qsharp-functions)
		  (qsharp-regexps font-lock-type-face qsharp-types)
		  '(("\\(mutable\\|let\\|set\\) \\(\\w+\\)\\b" . (2 font-lock-variable-name-face)))
		  '(("new \\(\\w+\\)\\b" . (1 font-lock-type-face)))
		  '(("\\b\\w+ : \\(\\w+\\)\\b" . (1 font-lock-type-face)))))

;;;###autoload
(define-derived-mode qsharp-mode fundamental-mode "Q# mode"
  "major mode for editing Q# code"
  :syntax-table qsharp-mode-syntax-table
  (setq font-lock-defaults '(qsharp-highlightings)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qs\\'" . qsharp-mode))

(provide 'qsharp-mode)
;;; qsharp-mode.el ends here
