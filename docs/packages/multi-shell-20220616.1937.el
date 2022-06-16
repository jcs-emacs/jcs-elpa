;;; multi-shell.el --- Managing multiple shell buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022  Shen, Jen-Chieh
;; Created date 2019-10-28 16:46:14

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/multi-shell
;; Package-Version: 20220616.1937
;; Package-Commit: ccd56dd3332086015418c5b3b6c73baded3cf69c
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: multiple shell terminal

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
;; Managing multiple shell buffers.
;;

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'subr-x)

(defgroup multi-shell nil
  "Managing multiple shell buffers in Emacs."
  :prefix "multi-shell-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/multi-shell"))

(defcustom multi-shell-prefer-shell-type 'shell
  "Prefer shell type."
  :type '(choice (const :tag "shell" shell)
                 (const :tag "eshell" eshell))
  :group 'multi-shell)

(defcustom multi-shell-display-function #'switch-to-buffer
  "Function to display shell buffer."
  :type 'function
  :group 'multi-shell)

(defvar multi-shell--current-shell-id 0
  "Record the shell id.")

(defvar multi-shell--live-shells nil
  "Record of list of shell that are still alive.")

(defvar multi-shell--prevent-nested-kill nil
  "Flag to prevent nested kill buffer command.")

(defun multi-shell--run-shell-procss-by-type ()
  "Run the shell process by current type."
  (save-window-excursion
    (cl-case multi-shell-prefer-shell-type
      (`shell (shell))
      (`eshell (eshell))
      (t (user-error "[ERROR] Invalid shell type: %s" multi-shell-prefer-shell-type)))))

(defun multi-shell--form-name (base)
  "Form the shell name by BASE."
  (format "*%s*" base))

(defun multi-shell--form-name-by-id (id &optional base)
  "Form the shell name by BASE and ID."
  (format "*%s: <%s>*" (or base multi-shell-prefer-shell-type) id))

(defun multi-shell--prefix-name ()
  "Return shell name's prefix."
  (format "*%s: <" multi-shell-prefer-shell-type))

(defun multi-shell--next-valid-index ()
  "Return next valid index to generate new shell."
  (let ((index 0))
    (while (get-buffer (multi-shell--form-name-by-id index)) (cl-incf index))
    index))

(defun multi-shell--cycle-delta-live-shell-list (st val)
  "Cycle through the live shell list the delta VAL and ST."
  (let ((target-index (+ st val)))
    (cond
     ((< target-index 0)
      (setq target-index (+ (length multi-shell--live-shells) target-index)))
     ((>= target-index (length multi-shell--live-shells))
      (setq target-index (% target-index (length multi-shell--live-shells)))))
    target-index))

(defun multi-shell--get-current-shell-index-by-id (&optional id)
  "Return the current shell index by ID."
  (unless id
    (setq multi-shell--current-shell-id (multi-shell--name-to-id (buffer-name))
          id multi-shell--current-shell-id))
  (let ((index 0) (break nil) (sp nil) (fn-index -1))
    (while (and (< index (length multi-shell--live-shells))
                (not break))
      (setq sp (nth index multi-shell--live-shells))
      (when (= (car sp) id)
        (setq fn-index index
              break t))
      (setq index (1+ index)))
    fn-index))

(defun multi-shell-select-list ()
  "Return the list of shell select."
  (let (fn-lst)
    (dolist (sp multi-shell--live-shells)
      (push (multi-shell--form-name-by-id (car sp)) fn-lst))
    fn-lst))

(defun multi-shell--name-to-id (sp-name)
  "Turn SP-NAME to id."
  (let ((start (length (multi-shell--prefix-name))))
    (string-to-number (substring sp-name start (length sp-name)))))

(defun multi-shell--correct-buffer-name (killed-id)
  "Correct the buffer name by moving the id above the KILLED-ID."
  (setq multi-shell--live-shells (reverse multi-shell--live-shells))
  (dolist (sp multi-shell--live-shells)
    (with-current-buffer (cdr sp)
      (let* ((shell-id (multi-shell--name-to-id (buffer-name)))
             (new-shell-id (1- shell-id)))
        (when (>= shell-id killed-id)
          (rename-buffer (multi-shell--form-name-by-id new-shell-id))))))
  (let ((index 0))
    (dolist (sp multi-shell--live-shells)
      (let ((new-id (multi-shell--name-to-id (with-current-buffer (cdr sp) (buffer-name)))))
        (setf (nth index multi-shell--live-shells) (cons new-id (cdr sp))))
      (setq index (1+ index))))
  (setq multi-shell--live-shells (reverse multi-shell--live-shells)))

;;;###autoload
(defun multi-shell-live-p ()
  "Check if any shell is alive."
  (not (= (length multi-shell--live-shells) 0)))

(defun multi-shell--switch (delta)
  "Switch to DELTA shell."
  (let* ((cur-index (multi-shell--get-current-shell-index-by-id))
         (sp-index (multi-shell--cycle-delta-live-shell-list cur-index (- 0 delta)))
         (sp (nth sp-index multi-shell--live-shells)))
    (multi-shell-select (multi-shell--form-name-by-id (car sp)))))

;;;###autoload
(defun multi-shell-prev ()
  "Switch to previous shell buffer."
  (interactive)
  (multi-shell--switch -1))

;;;###autoload
(defun multi-shell-next ()
  "Switch to next shell buffer."
  (interactive)
  (multi-shell--switch 1))

;;;###autoload
(defun multi-shell-select (sp-name)
  "Switch to shell buffer by SP-NAME."
  (interactive
   (list (completing-read "Select shell process: " (multi-shell-select-list))))
  (setq multi-shell--current-shell-id (multi-shell--name-to-id sp-name))
  (switch-to-buffer sp-name)
  sp-name)

;;;###autoload
(defun multi-shell-kill-all ()
  "Kill all shell buffers."
  (interactive)
  (while (not (= 0 (length multi-shell--live-shells)))
    (multi-shell-kill (nth 0 multi-shell--live-shells))))

;;;###autoload
(defun multi-shell-kill (&optional sp)
  "Kill the current shell buffer SP."
  (interactive)
  (when-let ((sp (or sp (nth (multi-shell--get-current-shell-index-by-id) multi-shell--live-shells))))
    (when (buffer-name (cdr sp))
      (with-current-buffer (cdr sp) (comint-kill-region (point-min) (point-max)))
      (kill-buffer (cdr sp)))
    (setq multi-shell--live-shells (remove sp multi-shell--live-shells))
    (multi-shell--correct-buffer-name multi-shell--current-shell-id)))

;;;###autoload
(defun multi-shell ()
  "Create a new shell buffer."
  (interactive)
  (let* ((id (length multi-shell--live-shells))
         (name (multi-shell--form-name-by-id id))
         (sh-name (multi-shell--form-name multi-shell-prefer-shell-type)))
    (unless (get-buffer sh-name) (multi-shell--run-shell-procss-by-type))
    (with-current-buffer sh-name
      (rename-buffer name)
      (when truncate-lines (toggle-truncate-lines) (message ""))
      (push (cons id (current-buffer)) multi-shell--live-shells))
    (funcall multi-shell-display-function name)))

(defun multi-shell--kill-buffer (fnc &rest args)
  "Advice execute around `kill-buffer' function with FNC and ARGS."
  (if (and (string-match-p (multi-shell--prefix-name) (buffer-name))
           (not multi-shell--prevent-nested-kill))
      (let ((multi-shell--prevent-nested-kill t))
        (multi-shell-kill))
    (apply fnc args)))
(advice-add 'kill-buffer :around #'multi-shell--kill-buffer)

;;
;; (@* "Externals" )
;;

(with-eval-after-load 'shell-pop
  (defvar shell-pop-internal-mode-buffer)
  (defvar shell-pop-last-shell-buffer-index)
  (defvar shell-pop-last-shell-buffer-name)

  (advice-add 'shell-pop--shell-buffer-name :override
              (lambda (index)
                (if (string-match-p "*\\'" shell-pop-internal-mode-buffer)
                    (format "*%s: <%d>*" multi-shell-prefer-shell-type index)
                  (format "%s%d" shell-pop-internal-mode-buffer index))))

  (advice-add 'multi-shell-select :after
              (lambda (&rest _)
                (let ((id multi-shell--current-shell-id))
                  (setq shell-pop-last-shell-buffer-index id
                        shell-pop-last-shell-buffer-name (multi-shell--form-name-by-id id))))))

(provide 'multi-shell)
;;; multi-shell.el ends here
