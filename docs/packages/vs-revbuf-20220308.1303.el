;;; vs-revbuf.el --- Revert buffers like Visual Studio  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-03-08 19:54:08

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Revert buffers like Visual Studio.
;; Keyword: revert vs
;; Version: 0.1.0
;; Package-Version: 20220308.1303
;; Package-Commit: 881610a2ed7f8e27ca8ae098d91bd616f5573b0e
;; Package-Requires: ((emacs "26.1") (fextern "0.1.0"))
;; URL: https://github.com/emacs-vs/vs-revbuf

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Revert buffers like Visual Studio.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'fextern)

(defconst vs-revbuf--msg-edit-extern "
The file has been changed externally, and has no unsaved changes inside this editor.
Do you want to reload it? "
  "Message to display when only edit externally.")

(defconst vs-revbuf--msg-edit-extern-and-unsaved "
The file has unsaved changes inside this editor and has been changed externally.
Do you want to reload it and lose the changes made in this source editor? "
  "Message to display when edit externally and there are unsaved changes.")

(defvar vs-revbuf--interactive-p nil
  "Internal use only.")

;;
;; (@* "Externals" )
;;

(declare-function flycheck-mode "ext:flycheck.el")
(declare-function page-break-lines-mode "ext:page-break-lines.el")

;;
;; (@* "Util" )
;;

(defun vs-revbuf--invalid-buffer-p (&optional buffer)
  "Return non-nil if BUFFER does't exist on disk but has a valid file path.
This occurs when file was opened but has moved to somewhere else externally."
  (when-let ((bfn (buffer-file-name buffer))) (not (file-exists-p bfn))))

(defun vs-revbuf--invalid-buffer-list ()
  "Return a list of invalid buffers."
  (cl-remove-if-not #'vs-revbuf--invalid-buffer-p (buffer-list)))

;;
;; (@* "Core" )
;;

;;;###autoload
(defun vs-revbuf-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  ;; Record all the enabled mode that you want to remain enabled after
  ;; revert the file.
  (let ((was-readonly (if buffer-read-only 1 -1))
        (was-g-hl-line (if (and (featurep 'hl-line) global-hl-line-mode) 1 -1))
        (was-flycheck (if (and (featurep 'flycheck) flycheck-mode) 1 -1))
        (was-page-lines (if (and (featurep 'page-break-lines) page-break-lines-mode) 1 -1)))
    ;; Revert it!
    (ignore-errors (revert-buffer :ignore-auto :noconfirm :preserve-modes))
    (fextern-update-buffer-save-string)
    (when (and (featurep 'line-reminder)
               (or (called-interactively-p 'interactive)
                   vs-revbuf--interactive-p))
      (line-reminder-clear-reminder-lines-sign))
    ;; Revert all the enabled modes
    (read-only-mode was-readonly)
    (when (featurep 'hl-line) (global-hl-line-mode was-g-hl-line))
    (when (featurep 'flycheck) (flycheck-mode was-flycheck))
    (when (featurep 'page-break-lines) (page-break-lines-mode was-page-lines))))

(defun vs-revbuf--all-invalid-buffers ()
  "Revert all invalid buffers."
  (dolist (buf (vs-revbuf--invalid-buffer-list))
    (with-current-buffer buf
      (when fextern-buffer-save-string-md5  ; this present only after first save!
        (set-buffer-modified-p nil)
        (let (kill-buffer-query-functions) (kill-this-buffer))))))

(defun vs-revbuf--all-valid-buffers ()
  "Revert all valid buffers."
  (dolist (buf (fextern--valid-buffer-list))
    (with-current-buffer buf
      (unless (buffer-modified-p) (vs-revbuf-no-confirm)))))

(defun vs-revbuf-ask-all (bufs &optional index)
  "Ask to revert all buffers decided by ANSWER.

This is called when only buffer changes externally and there are modification
still in this editor.

Optional argument INDEX is used to loop through BUFS."
  (when-let*
      ((vs-revbuf--interactive-p t)
       (index (or index 0)) (buf (nth index bufs))
       (path (buffer-file-name buf))
       (prompt (concat path "\n"
                       (if (buffer-modified-p buf)
                           vs-revbuf--msg-edit-extern-and-unsaved
                         vs-revbuf--msg-edit-extern)))
       (answer (completing-read prompt '("Yes" "Yes to All" "No" "No to All"))))
    (cl-incf index)
    (pcase answer
      ("Yes"
       (with-current-buffer buf (vs-revbuf-no-confirm))
       (vs-revbuf-ask-all bufs index))
      ("Yes to All"
       (vs-revbuf--all-valid-buffers)
       (vs-revbuf--all-invalid-buffers))
      ("No" (vs-revbuf-ask-all bufs index))
      ("No to All"))))  ; Does nothing, exit

;;;###autoload
(defun vs-revbuf-all ()
  "Refresh all open file buffers without confirmation."
  (interactive)
  (if-let ((bufs (fextern-buffers-edit-externally)))
      (vs-revbuf-ask-all bufs)
    (let ((vs-revbuf--interactive-p (called-interactively-p 'interactive)))
      (vs-revbuf--all-valid-buffers)
      (vs-revbuf--all-invalid-buffers))))

(provide 'vs-revbuf)
;;; vs-revbuf.el ends here
