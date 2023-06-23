;;; vbscript-mode --- VBScript major mode for visual basic programming in Emacs

;; Copyright: (C) 2009 Charles Sebold
;; Author: Charles Sebold <csebold at gmail.com>
;; Maintainer: Charles Sebold <csebold at gmail.com>
;; URL: http://www.emacswiki.org/emacs/VbsReplMode
;; Package-Version: 20160720.820
;; Package-Commit: 483dd5a2c8c4675ddb3c32fdd93146c6d248285d
;; Keywords: vbscript, repl, programming, mode
;; Created: Thu 19 Feb 2009 10:00
;; Version: 2.1

;;  This program is NOT part of GNU Emacs.
;; 
;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License as
;;  published by the Free Software Foundation; either version 2 of
;;  the License, or (at your option) any later version.
;;  
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;  GNU General Public License for more details.
;;  
;;  You should have received a copy of the GNU General Public License
;;  along with GNU Emacs; if not, write to the Free Software
;;  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;  02110-1301 USA
;;
;;; Commentary:

;; Note: for original author's details see:
;; [VbsReplMode](http://www.emacswiki.org/emacs/VbsReplMode)

;; Separated inferior repl from major mode, added company keyword support.

;; Usage:
;;
;; To use this, put it in your load-path and do something like the
;; following in your .emacs file:
;;
;;    ; VBScript editing
;;    (setq auto-mode-alist
;;       (append '(("\\.\\(vbs\\|wsf\\)$" . vbscript-mode))
;;               auto-mode-alist))
;; 
;; To start the repl, load a VBScript file, make sure you're in
;; visual-basic mode, and hit C-c C-z (or whatever you mapped to
;; vbs-setup-repl).  To execute from the beginning of the line (or the
;; beginning of the statement; it does recognize multiline VBScript),
;; use C-c C-e.  Select a region and execute it with C-c C-r.  If you
;; want it to Eval rather than Execute (like, for example, you want to
;; know what "x & y" would be), Use C-u before the above keystrokes.

;;; Code:
(eval-when-compile (require 'vbs-repl))
(require 'visual-basic-mode)
(require 'vbs-repl)
(declare-function visual-basic-keyword-upper-lower "visual-basic-mode")
(defvar company-keywords-alist)

(defgroup vbscript nil
  "Visual basic scripting mode."
  :group 'languages
  :prefix "vbscript-")

;; ------------------------------------------------------------
;;* Font-locking / keywords

;; This is some approximation of the set of reserved words in VBScript.
(eval-and-compile
  (defvar vbscript-all-keywords
    '("Abs" "And" "Array" "Asc" "AscB" "AscW" "Assignments" "Atn"
      "CBool" "CByte" "CCur" "CDate" "CDbl" "CInt" "CLng" "CSng" "CStr"
      "Call" "Case" "Chr" "ChrB" "ChrW" "Cint" "Class" "Clear" "Const"
      "Conversions" "Cos" "CreateObject" "Date" "DateAdd" "DateDiff"
      "DatePart" "DateSerial" "DateValue" "Day" "Derived" "Description"
      "Dim" "Do" "Each" "Else" "ElseIf" "Empty" "End" "Eqv" "Erase" "Err"
      "Error" "Eval" "Execute" "ExecuteGlobal" "Exit" "Exp" "Explicit"
      "False" "Filter" "FirstIndex" "Fix" "Fixs" "For" "FormatCurrency"
      "FormatDateTime" "FormatNumber" "FormatPercent" "Function" "GetLocale"
      "GetObject" "GetRef" "Global" "HelpContext" "HelpFile" "Hex" "Hour"
      "If" "IgnoreCase" "Imp" "InStr" "InStrB" "InStrRev" "InputBox" "Instr"
      "Int" "Int," "Is" "IsArray" "IsArray " "IsDate" "IsEmpty" "IsNull"
      "IsNumeric" "IsObject" "Join" "LBound" "LCase" "LTrim" "Lcase" "Left"
      "LeftB" "Len" "LenB" "Length" "Let" "LoadPicture" "Log" "Loop" "Ltrim"
      "Match" "Math" "Maths" "Mid" "MidB" "Minute" "Mod" "Month" "MonthName"
      "MsgBox" "New" "Next" "Not" "Nothing" "Now" "Null" "Number" "Oct" "On"
      "Option" "Or" "Pattern" "Private" "Procedures" "Public" "RGB" "RTrim"
      "Raise" "Randomize" "ReDim" "Rem" "Replace" "Right" "RightB" "Rnd"
      "Round" "Rtrim" "ScriptEngine" "ScriptEngineBuildVersion"
      "ScriptEngineMajorVersion" "ScriptEngineMinorVersion" "Second"
      "Select" "Set" "SetLocale" "Sgn" "Sin" "Source" "Space" "Split" "Sqr"
      "StrComp" "String" "Strings" "Sub" "SubMatches" "Tan" "Test" "Then"
      "Time" "TimeSerial" "TimeValue" "Timer" "Trim" "Trims" "True"
      "TypeName" "UBound" "UCase" "Ucase" "Until" "Value" "VarType"
      "Variants" "VbCrLf" "Weekday" "WeekdayName" "Wend" "While" "With"
      "Xor" "Year" "vbAbort" "vbAbortRetryIgnore" "vbApplicationModal"
      "vbArray" "vbBinaryCompare" "vbBlack" "vbBlue" "vbBoolean" "vbByte"
      "vbCancel" "vbCr" "vbCritical" "vbCurrency" "vbCyan" "vbDataObject"
      "vbDate" "vbDecimal" "vbDefaultButton1" "vbDefaultButton2"
      "vbDefaultButton3" "vbDefaultButton4" "vbEmpty" "vbError"
      "vbExclamation" "vbFalse" "vbFirstFourDays" "vbFirstFullWeek"
      "vbFirstJan1" "vbFormFeed" "vbFriday" "vbGeneralDate" "vbGreen"
      "vbIgnore" "vbInformation" "vbInteger" "vbLf" "vbLong" "vbLongDate"
      "vbLongTime" "vbMagenta" "vbMonday" "vbNewLine" "vbNo" "vbNull"
      "vbNullChar" "vbNullString" "vbOK" "vbOKCancel" "vbOKOnly" "vbObject"
      "vbObjectError" "vbQuestion" "vbRed" "vbRetry" "vbRetryCancel"
      "vbSaturday" "vbShortDate" "vbShortTime" "vbSingle" "vbString"
      "vbSunday" "vbSystemModal" "vbTab" "vbTextCompare" "vbThursday"
      "vbTrue" "vbTuesday" "vbUseDefault" "vbUseSystemDayOfWeek" "vbVariant"
      "vbVerticalTab" "vbWednesday" "vbWhite" "vbYellow" "vbYes" "vbYesNo"
      "vbYesNoCancel")))

(defvar vbscript-font-lock-keywords-1
  (eval-when-compile
    (list
     ;; Names of functions.
     (list visual-basic-defun-start-regexp
           '(1 font-lock-keyword-face nil t)
           '(2 font-lock-keyword-face nil t)
           '(3 font-lock-function-name-face))

     ;; Statement labels
     (cons visual-basic-label-regexp 'font-lock-keyword-face)

     ;; Case values
     ;; String-valued cases get font-lock-string-face regardless.
     (list "^[ \t]*case[ \t]+\\([^'\n]+\\)" 1 'font-lock-keyword-face t)

     ;; Any keywords you like.
     (list (regexp-opt
            '("Dim" "If" "Then" "Else" "ElseIf" "End If") 'words)
           1 'font-lock-keyword-face))))

(defvar vbscript-font-lock-keywords-2
  (append vbscript-font-lock-keywords-1
          (eval-when-compile
            `((, (regexp-opt vbscript-all-keywords 'words)
                   1 font-lock-keyword-face)))))

(defvar vbscript-font-lock-keywords vbscript-font-lock-keywords-1)


;; ------------------------------------------------------------
;;* Major Mode
;; ------------------------------------------------------------

;;* Menu
(defvar vbscript-menu
  '("VBScript"
    ["Start REPL" vbs-repl :keys "C-c C-z"]
    ["Evaluate Expression" vbs-repl-execute
     :help "Evaluate expression before point in REPL" :keys "C-c C-e"]
    ["Execute Region" vbs-execute-region
     :help "Evaluate region in REPL" :keys "C-c C-r"]
    ["Execute Sub" vbs-repl-execute-sub
     :help "Evaluate current Sub or Function" :keys "C-M-x"]))

;;* Map
(defvar vbscript-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil vbscript-menu)
    (define-key map (kbd "C-c C-z") #'vbs-repl)
    (define-key map (kbd "C-c C-e") #'vbs-repl-execute)
    (define-key map (kbd "C-c C-r") #'vbs-repl-execute-region)
    (define-key map (kbd "C-M-x")   #'vbs-repl-execute-sub)
    map))

;;;###autoload
(define-derived-mode vbscript-mode visual-basic-mode "VBScript"
  "Major mode for VBScript.
\\{vbscript-mode-map}"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        `((vbscript-font-lock-keywords
           vbscript-font-lock-keywords-1
           vbscript-font-lock-keywords-2)
          nil t ((,(string-to-char "_") . "w"))))

  ;; add company-keyword completion
  (when (featurep 'company)
    (make-local-variable 'company-keywords-alist)
    (push (append '(vbscript-mode)
                  (visual-basic-keyword-upper-lower vbscript-all-keywords))
          company-keywords-alist)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(vbs?\\|wsf\\)$" . vbscript-mode))

;;; vbscript-mode.el ends here
