;;; vbs-repl.el --- Run visual basic repl as inferior process

;; Copyright: (C) 2009 Charles Sebold
;; Author: Charles Sebold <csebold at gmail.com>
;; Maintainer: Charles Sebold <csebold at gmail.com>
;; URL: http://www.emacswiki.org/emacs/VbsReplMode
;; Package-Version: 20160720.820
;; Package-Commit: 483dd5a2c8c4675ddb3c32fdd93146c6d248285d
;; Keywords: vbscript, repl, programming, mode

;;
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

;; Separated repl out from vbscript mode.

;; See: [VbsReplMode](http://www.emacswiki.org/emacs/VbsReplMode)

;;; Code:

(require 'comint)

(defgroup vbs-repl nil
  "Run visual basic repl."
  :group 'languages
  :prefix "vbs-repl-")

;; ------------------------------------------------------------

(defvar vbs-repl-script "ReplPrompt = \"> \"
Set ReplFso = CreateObject(\"Scripting.FileSystemObject\")

Sub ReplErrprint(errno)
    WScript.StdOut.Write \"Error # \" & CStr(errno.Number) & \" \" & _
        errno.Description
End Sub

Function ReplWhyMulti(firstLine)
    If Right(firstLine,1) = \"_\" Then
        ReplWhyMulti = \"_\"
    Elseif LCase(Left(firstLine,3)) = \"sub\" Then
        ReplWhyMulti = \"Sub\"
    Elseif LCase(Left(firstLine,8)) = \"function\" Then
        ReplWhyMulti = \"Function\"
    Else
        ReplWhyMulti = \"?\"
    End If
End Function

Function ReplTestEndMulti(lastLine,why)
    lastLine = LTrim(RTrim(lastLine))
    If why = \"_\" And Right(lastLine,1) <> \"_\" Then
        ReplTestEndMulti = True
    Elseif why = \"Sub\" And LCase(lastLine) = \"end sub\" Then
        ReplTestEndMulti = True
    Elseif why = \"Function\" And LCase(lastline) = \"end function\" Then
        ReplTestEndMulti = True
    Else
        ReplTestEndMulti = False
    End If
End Function

Function ReplBufferIn(firstLine)
    firstLine = LTrim(RTrim(firstLine))
    ReplWhy = ReplWhyMulti(firstLine)
    ReplPrompt = ReplWhy & \"> \"
    ReplBufferIn = firstLine
    Do
        WScript.StdOut.Write ReplPrompt
        nextLine = WScript.StdIn.ReadLine
        ReplBufferIn = ReplBufferIn & vbCRLF & nextLine
    Loop Until ReplTestEndMulti(nextLine, ReplWhy)
    ReplPrompt = \"> \"
End Function

While 1:
    WScript.StdOut.Write ReplPrompt
    ReplInput = WScript.StdIn.ReadLine
    If ReplInput <> \"\" Then
        If ReplWhyMulti(ReplInput) <> \"?\" Then
            ReplInput = ReplBufferIn(ReplInput)
            On Error Resume Next
            Execute(ReplInput)
        Else
            ReplInputArgs = Split(LTrim(ReplInput))
            If ReplInputArgs(0) = \"ReplLoad\" Then
                Set ReplFile = ReplFso.OpenTextFile(ReplInputArgs(1), 1)
                ReplInput = \"\"
                Do While ReplFile.AtEndOfStream <> True
                    ReplThisLine = ReplFile.ReadLine
                    ReplInput = ReplInput & ReplThisLine & vbCRLF
                Loop
                ReplFile.Close()
                On Error Resume Next
                Execute(ReplInput)
            Elseif ReplInputArgs(0) = \"ReplLoadEval\" Then
                Set ReplFile = ReplFso.OpenTextFile(ReplInputArgs(1), 1)
                ReplInput = \"\"
                Do While ReplFile.AtEndOfStream <> True
                    ReplThisLine = ReplFile.ReadLine
                    ReplInput = ReplInput & ReplThisLine & vbCRLF
                Loop
                ReplFile.Close()
                On Error Resume Next
                WScript.StdOut.Write Eval(ReplInput) & vbCRLF
            Elseif ReplInputArgs(0) = \"ReplEval\" Then
                On Error Resume Next
                WScript.StdOut.Write Eval(Right(ReplInput,Len(ReplInput)-9)) & _
                       vbCRLF
            Else
                On Error Resume Next
                Execute(ReplInput)
            End If
        End If
        If Err.Number <> 0 Then
            ReplErrprint(Err)
            WScript.StdOut.Write vbCRLF
            Err.Clear
        End If
    End If
    On Error GoTo 0
Wend
")

;; ------------------------------------------------------------
;;* Internal Functions

(defun vbs-repl-execute-expression (arg evalp)
  "Send expression to REPL and evaluate it.
Argument ARG is the prefix argument.
Argument EVALP is whether to sent this to Eval (nil) or Execute (t)."
  (if evalp
      (if (string-match "\n" arg)
          (let ((tempfile (make-temp-file "vbsrepl" nil ".vbs")))
            (with-temp-file tempfile
              (insert arg))
            (comint-send-string "*VBScript*"
                                (concat "ReplLoadEval " tempfile "\n")))
        (comint-send-string "*VBScript*" (concat "ReplEval " arg "\n")))
    (if (string-match "\n" arg)
        (let ((tempfile (make-temp-file "vbsrepl" nil ".vbs")))
          (with-temp-file tempfile
            (insert arg))
          (comint-send-string "*VBScript*"
                              (concat "ReplLoad " tempfile "\n")))
      (comint-send-string "*VBScript*" (concat arg "\n")))))

(defun vbs-repl-last-exp (arg)
  "Return last expression in current buffer before ARG."
  (let ((beginning-pt
         (let ((possible (save-excursion
                           (beginning-of-line)
                           (point))))
           (if (= possible (point-min))
               possible
             (save-excursion
               (while
                   (progn
                     (forward-line -1)
                     (beginning-of-line)
                     (and (not (= (point) (point-min)))
                          (looking-at "[^\n]*?_ *\n"))))
               (if (and (= (point) (point-min))
                        (looking-at "[^\n]*?_ *\n"))
                   (point)
                 (forward-line 1)
                 (point)))))))
    (buffer-substring-no-properties beginning-pt arg)))

(defun vbs-repl-run-repl ()
  "Deletes all tempfiles and run a new REPL."
  (interactive)
  (dolist (i (directory-files temporary-file-directory t
                              "vbsrepl.*\\.vbs"))
    (delete-file i))
  (let ((tempfile (make-temp-file "vbsrepl" nil ".vbs")))
    (with-temp-file tempfile
      (insert vbs-repl-script))
    (make-comint "VBScript" "cscript" nil tempfile)))


;; ------------------------------------------------------------
;;* User Functions

;;;###autoload
(defalias 'vbs-repl 'vbs-repl-setup-repl)

(defun vbs-repl-setup-repl ()
  "Set up a REPL in the opposite window, restarting it if necessary."
  (interactive)
  (delete-other-windows)
  (if (> (frame-parameter nil 'width) 160)
      (split-window-horizontally)
    (split-window-vertically))
  (other-window 1)
  (if (and (get-buffer "*VBScript*") (get-process "VBScript"))
      (switch-to-buffer "*VBScript*")
    (vbs-repl-run-repl)
    (switch-to-buffer "*VBScript*"))
  (other-window 1))

(defun vbs-repl-execute-sub ()
  "When within a Sub or Function definition, Execute it."
  (interactive)
  (save-excursion
    (let* ((current (point))
           (end (progn
                  (beginning-of-line)
                  (re-search-forward "^ *end \\(sub\\|function\\)" nil t)
                  (point)))
           (s-or-f (match-string-no-properties 1))
           (start (progn
                    (re-search-backward
                     (concat "^ *" s-or-f " ") nil t)
                    (point))))
      (if (> start current)
          (message "Not in a sub or function.")
        (vbs-repl-execute-expression
         (buffer-substring-no-properties start end)
         nil)))))

(defun vbs-repl-execute-region (start end)
  "Send region from START to END to REPL."
  (interactive "r")
  (vbs-repl-execute-expression
   (buffer-substring-no-properties start end)
   current-prefix-arg))

(defun vbs-repl-execute (arg)
  "Evaluate VBScript expression before point.
Argument ARG is the prefix arg, add prefix if you want this Evaled
rather than Executed."
  (interactive "P")
  (let ((eval-this (vbs-repl-last-exp (point))))
    (if eval-this
        (vbs-repl-execute-expression eval-this arg)
      (message "Unable to find last expression."))))

(provide 'vbs-repl)

;;; vbs-repl.el ends here
