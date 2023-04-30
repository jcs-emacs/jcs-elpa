;;; qsharp-mode.el --- Programming mode for the Q# language.
;; Author: forked-from-1kasper
;; Keywords: languages, q#, quantum
;; Package-Version: 20190416.931
;; Package-Commit: 1e52e85fa516a20a568420d60b43fb7b22f49efc
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;  Add a Q# mode to Emacs.

;;; Code:

(defconst qsharp-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\" "\"" table)
    table))

(defun regexp-keyword (name)
  ((concat "\\b" name "\\b") . font-lock-keyword-face))

(defun make-big-keyword (names)
  (mapconcat
    #'(lambda (name) (concat "\\b" name "\\b")) names "\\|"))

(defun regexps (some-lock names)
  `((,(make-big-keyword names) . ,some-lock)))

(defconst qsharp-keywords '("set" "mutable" "let" "body" "operation" "using" "open" "namespace" "newtype" "adjoint" "auto" "controlled" "function" "new"))
(defconst qsharp-constants '("One" "Zero" "PauliX" "PauliY" "PauliZ" "PauliI" "true" "false"))
(defconst qsharp-builtins '("if" "elif" "else" "fail" "for" "in" "return" "repeat" "until" "fixup"))
(defconst qsharp-functions '("M" "H" "CNOT" "CCNOT" "I" "Measure" "Message" "Assert" "AssertProb" "Exp" "ExpFrac" "R" "R1" "RFrac" "R1Frac" "Random" "Reset" "ResetAll" "Rx" "Ry" "Rz" "S" "SWAP" "T" "X" "Y" "Z" "MultiX" "Length"))
(defconst qsharp-types '("Int" "Double" "String" "Bool" "Qubit" "Pauli" "Result" "Range"))

(defconst qsharp-highlightings
  (append (regexps font-lock-keyword-face qsharp-keywords)
	  (regexps font-lock-constant-face qsharp-constants)
	  (regexps font-lock-builtin-face qsharp-builtins)
	  (regexps font-lock-function-name-face qsharp-functions)
	  (regexps font-lock-type-face qsharp-types)
	  '(("\\(mutable\\|let\\|set\\) \\(\\w+\\)\\b" . (2 font-lock-variable-name-face)))
	  '(("new \\(\\w+\\)\\b" . (1 font-lock-type-face)))
	  '(("\\b\\w+ : \\(\\w+\\)\\b" . (1 font-lock-type-face)))))

(define-derived-mode qsharp-mode fundamental-mode "Q# mode"
  "major mode for editing Q# code"
  :syntax-table qsharp-mode-syntax-table
  (setq font-lock-defaults '(qsharp-highlightings)))

(provide 'qsharp-mode)

;;; qsharp-mode.el ends here
