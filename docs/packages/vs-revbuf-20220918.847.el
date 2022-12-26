;;; vs-revbuf.el --- Revert buffers like Visual Studio  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-03-08 19:54:08

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/vs-revbuf
;; Package-Version: 20220918.847
;; Package-Commit: 1858cd77bd61e5219c9b439b4550c48a7d5dc547
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1") (fextern "0.1.0"))
;; Keywords: convenience revert vs

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

(defgroup vs-revbuf nil
  "Revert buffers like Visual Studio."
  :prefix "vs-revbuf-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/vs-revbuf"))

(defcustom vs-revbuf-ask-unsaved-changes-only nil
  "Ask only when there are unsaved changes."
  :type 'boolean
  :group 'vs-revbuf)

(defcustom vs-revbuf-on-identical nil
  "Revert buffers even buffer and file (on dist) have the same content."
  :type 'boolean
  :group 'vs-revbuf)

(defconst vs-revbuf--msg-edit-extern "
The file has been changed externally, and has no unsaved changes inside this editor.
Do you want to reload it? "
  "Message when only edited externally.")

(defconst vs-revbuf--msg-edit-extern-and-unsaved "
The file has unsaved changes inside this editor and has been changed externally.
Do you want to reload it and lose the changes made in this source editor? "
  "Message when edited externally and there are unsaved changes.")

(defconst vs-revbuf--msg-edit-moved "
The file has been moved (not found) externally, and has no unsaved changes inside this editor.
Do you want to kill it? "
  "Message when buffer is modify, but file has moved externally.")

(defconst vs-revbuf--msg-edit-moved-and-unsaved "
The file has unsaved changes inside this editor and has been moved (not found) externally.
Do you want to kill it and lose the changes made in this source editor? "
  "Message when buffer is modify, but file has moved externally.")

(defvar vs-revbuf--interactive-p nil
  "Internal use only.")

;;
;; (@* "Externals" )
;;

(defvar flycheck-mode)
(defvar page-break-lines-mode)

(declare-function flycheck-mode "ext:flycheck.el")
(declare-function line-reminder-clear-reminder-lines-sign "ext:line-reminder.el")
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

(defun vs-revbuf--kill-buffer-no-confirm ()
  "Kill modified/unmodified buffer without confirming."
  (set-buffer-modified-p nil)
  (let (kill-buffer-query-functions) (kill-this-buffer)))

;;
;; (@* "Core" )
;;

(defun vs-revbuf--buffer-identical ()
  "Return t if the buffer is identical on the disk."
  (let ((buffer-md5 (md5 (buffer-string)))
        (file-md5 (md5 (fextern--file-content buffer-file-name))))
    (string= buffer-md5 file-md5)))

;;;###autoload
(defun vs-revbuf-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  ;; Record all the enabled mode that you want to remain enabled after
  ;; revert the file.
  (let ((was-readonly buffer-read-only)
        (was-flycheck (and (featurep 'flycheck) flycheck-mode))
        (was-page-lines (and (featurep 'page-break-lines) page-break-lines-mode)))
    ;; Revert it!
    (ignore-errors (revert-buffer :ignore-auto :noconfirm :preserve-modes))
    (fextern-update-buffer-save-string)
    (when (and (featurep 'line-reminder)
               (or vs-revbuf--interactive-p
                   (called-interactively-p 'interactive)))
      (line-reminder-clear-reminder-lines-sign))
    ;; Revert all the enabled modes
    (when was-readonly (read-only-mode 1))
    (when was-flycheck (flycheck-mode 1))
    (when was-page-lines (page-break-lines-mode 1))))

(defun vs-revbuf--all-invalid-buffers ()
  "Revert all invalid buffers."
  (dolist (buf (vs-revbuf--invalid-buffer-list))
    (with-current-buffer buf
      (unless fextern-buffer-newly-created  ; Execlude newly created buffer
        ;; If we hit here, the file has been moved externally...
        (if (buffer-modified-p)
            ;; There is unsaved changes!
            (when (yes-or-no-p (concat buffer-file-name "\n" vs-revbuf--msg-edit-moved-and-unsaved))
              (vs-revbuf--kill-buffer-no-confirm))
          ;; No unsaved changes
          (when (or vs-revbuf-ask-unsaved-changes-only
                    (yes-or-no-p (concat buffer-file-name "\n" vs-revbuf--msg-edit-moved)))
            (vs-revbuf--kill-buffer-no-confirm)))))))

(defun vs-revbuf--all-valid-buffers ()
  "Revert all valid buffers."
  (dolist (buf (fextern--valid-buffer-list))
    (with-current-buffer buf
      (when (and
             (or (not (buffer-modified-p)) vs-revbuf--interactive-p)
             ;; Handle identical condition
             (or vs-revbuf-on-identical
                 (not (vs-revbuf--buffer-identical))))
        (vs-revbuf-no-confirm)))))

(defun vs-revbuf-ask-all (bufs &optional index)
  "Ask to revert all buffers decided by ANSWER.

This is called when only buffer changes externally and there are modification
still in this editor.

Optional argument INDEX is used to loop through BUFS."
  (when-let*
      ((index (or index 0)) (buf (nth index bufs))
       (path (buffer-file-name buf)))
    (let* ((modified (buffer-modified-p buf))
           (prompt (concat path "\n"
                           (if modified vs-revbuf--msg-edit-extern-and-unsaved
                             vs-revbuf--msg-edit-extern)))
           (answer (if (and vs-revbuf-ask-unsaved-changes-only (not modified)) "Yes"
                     (completing-read prompt '("Yes" "Yes to All" "No" "No to All")))))
      (cl-incf index)
      (pcase answer
        ("Yes"
         (with-current-buffer buf (vs-revbuf-no-confirm))
         (vs-revbuf-ask-all bufs index))
        ("Yes to All"
         (vs-revbuf--all-valid-buffers)
         (vs-revbuf--all-invalid-buffers))
        ("No" (vs-revbuf-ask-all bufs index))
        ("No to All")))))  ; Does nothing, exit

;;;###autoload
(defun vs-revbuf-all ()
  "Refresh all open file buffers without confirmation."
  (interactive)
  (if-let ((bufs (fextern-buffers-edit-externally))
           (vs-revbuf--interactive-p t))
      (vs-revbuf-ask-all bufs)
    (let ((vs-revbuf--interactive-p (called-interactively-p 'interactive)))
      (vs-revbuf--all-valid-buffers)
      (vs-revbuf--all-invalid-buffers))))

(defun vs-revbuf--focus-in (&rest _)
  "Hook when focus in."
  (when (and (frame-focus-state) (null (active-minibuffer-window)))
    (vs-revbuf-all)))

(defun vs-revbuf-mode--enable ()
  "Enable function `vs-revbuf-mode'."
  (add-function :after after-focus-change-function #'vs-revbuf--focus-in))

(defun vs-revbuf-mode--disable ()
  "Disable function `vs-revbuf-mode'."
  (remove-function after-focus-change-function #'vs-revbuf--focus-in))

;;;###autoload
(define-minor-mode vs-revbuf-mode
  "Minor mode `vs-revbuf-mode'."
  :global t
  :require 'vs-revbuf-mode
  :group 'vs-revbuf
  (if vs-revbuf-mode (vs-revbuf-mode--enable) (vs-revbuf-mode--disable)))

(provide 'vs-revbuf)
;;; vs-revbuf.el ends here
