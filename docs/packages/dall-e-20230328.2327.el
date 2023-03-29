;;; dall-e.el --- Use DALL-E inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/dall-e
;; Package-Version: 20230328.2327
;; Package-Commit: 3432d9dd8558b3efc5daf00081a74fb1759eea9b
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (openai "0.1.0") (lv "0.0") (ht "2.0") (spinner "1.7.4") (reveal-in-folder "0.1.2"))
;; Keywords: comm dall-e

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
;; Use DALL-E inside Emacs.
;;

;;; Code:

(require 'cl-lib)
(require 'let-alist)
(require 'subr-x)

(require 'openai)
(require 'openai-image)
(require 'lv)
(require 'ht)
(require 'spinner)
(require 'reveal-in-folder)

(defgroup dall-e nil
  "Use DALL-E inside Emacs."
  :prefix "dall-e-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/dall-e"))

(defcustom dall-e-n 1
  "The number of images to generate.  Must be between 1 and 10."
  :type 'integer
  :group 'dall-e)

(defcustom dall-e-size "1024x1024"
  "The size of the generated images.

Must be one of `256x256', `512x512', or `1024x1024'."
  :type 'string
  :group 'dall-e)

(defcustom dall-e-spinner-type 'moon
  "The type of the spinner."
  :type '(choice (const :tag "Key to variable `spinner-types'" symbol)
                 (const :tag "Vector of characters" vector))
  :group 'openai)

(defcustom dall-e-cache-dir
  (expand-file-name (locate-user-emacs-file ".cache/dall-e"))
  "Absolute path to download image files."
  :risky t
  :type 'directory
  :group 'dall-e)

(defconst dall-e-buffer-name-format "*DALL-E: <%s>*"
  "Name of the buffer to use for the `dall-e' instance.")

(defvar dall-e-instances (ht-create)
  "List of instances, each pair is consist of (index . buffer).")

(defvar-local dall-e-instance nil
  "Instance data for each buffer.")

(defvar-local dall-e-requesting-p nil
  "Non-nil when requesting; waiting for the response.")

(defvar-local dall-e-spinner-counter 0
  "Spinner counter.")

(defvar-local dall-e-spinner-timer nil
  "Spinner timer.")

(defvar-local dall-e-tip-inserted-p nil
  "Use to erase tip after first input.")

(defvar-local dall-e-images nil
  "List of images for current session.")

(defface dall-e-user
  '((t :inherit font-lock-builtin-face))
  "Face used for user."
  :group 'dall-e)

(defface dall-e-tip
  '((t :foreground "#848484"))
  "Face used for tip."
  :group 'dall-e)

;;
;;; Util

(defun dall-e--cancel-timer (timer)
  "Cancel TIMER."
  (when (timerp timer)
    (cancel-timer timer)))

(defun dall-e--pop-to-buffer (buffer-or-name)
  "Wrapper to function `pop-to-buffer'.

Display buffer from BUFFER-OR-NAME."
  (pop-to-buffer buffer-or-name `((display-buffer-in-direction)
                                  (dedicated . t))))

(defun dall-e-user ()
  "Return the current user."
  (if (string-empty-p openai-user)
      "user"  ; this is free?
    openai-user))

(defun dall-e-cache-dir ()
  "Return cache directory for current session."
  (expand-file-name (openai--2str (car dall-e-instance)) dall-e-cache-dir))

;;
;;; Spinner

(defun dall-e--cancel-spinner-timer ()
  "Cancel spinner timer."
  (dall-e--cancel-timer dall-e-spinner-timer)
  (setq dall-e-spinner-timer nil))

(defun dall-e--start-spinner ()
  "Start spinner."
  (dall-e--cancel-spinner-timer)
  (setq dall-e-spinner-counter 0
        dall-e-spinner-timer (run-with-timer (/ spinner-frames-per-second 60.0)
                                             (/ spinner-frames-per-second 60.0)
                                             (lambda ()
                                               (cl-incf dall-e-spinner-counter)
                                               (force-mode-line-update)))))

;;
;;; Instances

(defmacro dall-e-with-instance (instance &rest body)
  "Execute BODY within INSTANCE."
  (declare (indent 1))
  `(when-let* ((buffer (and ,instance
                            (get-buffer (cdr ,instance))))
               ((buffer-live-p buffer)))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         ,@body))))

(defun dall-e--live-instances ()
  "Return a list of live instances."
  (let ((live-instances))
    (ht-map (lambda (_ buffer)
              (when (and (get-buffer buffer)
                         (buffer-live-p buffer))
                (push buffer live-instances)))
            dall-e-instances)
    (reverse live-instances)))

(defun dall-e--shown-instances ()
  "Return a list of live instances that are displayed on the screen."
  (let ((live-instances (dall-e--live-instances))
        (shown-instances))
    (dolist (instance live-instances)
      (when (get-buffer-window instance)
        (push instance shown-instances)))
    (reverse shown-instances)))

(defun dall-e--new-index ()
  "Find killed instance before giving new index."
  (let ((target))
    (cl-some (lambda (index)
               (let ((buffer (ht-get dall-e-instances index)))
                 (when (or (not (get-buffer buffer))
                           (not (buffer-live-p buffer)))  ; if buffer is killed
                   (setq target index)
                   t)))
             (ht-keys dall-e-instances))
    (unless target                               ; No killed instance?
      (setq target (ht-size dall-e-instances)))  ; Create a new one!
    target))

(defun dall-e-restart ()
  "Restart session."
  (interactive)
  (when (eq major-mode #'dall-e-mode)
    (let* ((instance dall-e-instance)
           (index    (car instance))
           (old-name))
      ;; If buffer is alive, kill it!
      (dall-e-with-instance instance
        (setq old-name (buffer-name))
        (kill-this-buffer))
      ;; `old-name' will remain `nil' if buffer is not killed or invalid!
      (when old-name
        (dall-e-register-instance index old-name)
        (switch-to-buffer old-name)))))

;;
;;; Input

(defun dall-e--fill-region (start end)
  "Like function `fill-region' (START to END), improve readability."
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (when (< fill-column (current-column))
        (fill-region (line-beginning-position) (line-end-position)))
      (forward-line 1))))

(defun dall-e-send-response (prompt)
  "Send PROMPT to DALL-E."
  (let ((user (dall-e-user))
        (instance dall-e-instance)
        (cache-dir (dall-e-cache-dir)))
    (when (string-empty-p prompt)
      (user-error "[INFO] Invalid prompt or description: %s" prompt))
    (dall-e-with-instance instance
      ;; clear up the tip message
      (when dall-e-tip-inserted-p
        (erase-buffer)
        (setq dall-e-tip-inserted-p nil))
      ;; Display input
      (let ((start (point))
            (role (format "<%s>:" user)))
        (add-face-text-property 0 (length role) 'dall-e-user nil role)
        (insert role " " prompt)
        (insert "\n\n")
        (dall-e--fill-region start (point))))
    (setq dall-e-requesting-p t)
    (dall-e--start-spinner)
    (openai-image prompt
                  (lambda (data)
                    (dall-e-with-instance instance
                      (setq dall-e-requesting-p nil)
                      (dall-e--cancel-spinner-timer)
                      (unless openai-error
                        (ignore-errors (make-directory cache-dir t))
                        (let-alist data
                          (mapc (lambda (images-data)
                                  (let-alist images-data
                                    (let* ((url .url)
                                           (name (file-name-nondirectory url))
                                           (abs (expand-file-name name cache-dir)))
                                      (url-copy-file name cache-dir)
                                      (push (cons abs url) dall-e-images))))
                                .data))
                        )))
                  :n dall-e-n
                  :size dall-e-size
                  :user user)))

(defun dall-e-type-response ()
  "Type response to OpenAI."
  (interactive)
  (cond
   (dall-e-requesting-p
    (message "[BUSY] Waiting for OpanAI to response..."))
   (t
    (dall-e-send-response (read-string "Type the detailed description: ")))))

;;
;;; Info

(defun dall-e--pre-command-once (&rest _)
  "One time pre-command after Easky command."
  ;; XXX: We pass on to next post-command!
  (remove-hook 'pre-command-hook #'dall-e--pre-command-once)
  (add-hook 'post-command-hook #'dall-e--post-command-once))

(defun dall-e--post-command-once ()
  "One time post-command after info command."
  ;; XXX: This will allow us to scroll in the lv's window!
  (unless (equal lv-wnd (selected-window))
    ;; Once we select window other than lv's window, then we kill it!
    (remove-hook 'post-command-hook #'dall-e--post-command-once)
    (lv-delete-window)))

(defun dall-e-info ()
  "Show session information."
  (interactive)
  (when (eq major-mode #'dall-e-mode)
    (lv-message
     (concat
      (format "session: %s" (cdr dall-e-instance)) "\n"
      (format "images: %s" (length dall-e-images))
      "\n\n"
      (format "n: %s" dall-e-n) "\n"
      (format "size: %s" dall-e-size) "\n"
      (format "user: %s" (dall-e-user))))
    ;; Register event to cancel lv window!
    (add-hook 'pre-command-hook #'dall-e--pre-command-once)))

;;
;;; Entry

(defun dall-e-reveal-cache-directory ()
  "Reveal cache directory in folder."
  (interactive)
  (when (eq major-mode #'dall-e-mode)
    (ignore-errors (make-directory (dall-e-cache-dir) t))
    (reveal-in-folder--signal-shell (dall-e-cache-dir))))

(defun dall-e-clear-cahce ()
  "Clear cache for current session."
  (interactive)
  (when (eq major-mode #'dall-e-mode)
    (ignore-errors (delete-directory (dall-e-cache-dir) t))))

(defun dall-e-mode--kill-buffer-hook ()
  "Kill buffer hook."
  (dall-e--cancel-spinner-timer)
  (dall-e-clear-cahce))

(defun dall-e-header-line ()
  "The display for header line."
  (format " %s[Session] %s  [Images] %s  [User] %s"
          (if dall-e-requesting-p
              (let* ((spinner (if (symbolp dall-e-spinner-type)
                                  (cdr (assoc dall-e-spinner-type spinner-types))
                                dall-e-spinner-type))
                     (len (length spinner)))
                (when (<= len dall-e-spinner-counter)
                  (setq dall-e-spinner-counter 0))
                (format "%s " (elt spinner dall-e-spinner-counter)))
            "")
          (cdr dall-e-instance)
          (length dall-e-images)
          (dall-e-user)))

(defvar dall-e-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'dall-e-type-response)
    map)
  "Keymap for `dall-e-mode'.")

(defun dall-e-mode-insert-tip ()
  "Insert tip to output buffer."
  (when (string-empty-p (buffer-string))
    (setq dall-e-tip-inserted-p t)
    (let ((inhibit-read-only t)
          (tip "Press <return> to start entering detailed description

`M-x dall-e-info` will print out more information about the current session!
"))
      (add-face-text-property 0 (length tip) 'dall-e-tip nil tip)
      (insert tip))))

;;;###autoload
(define-derived-mode dall-e-mode fundamental-mode "DALL-E"
  "Major mode for `dall-e-mode'.

\\<dall-e-mode-map>"
  (setq-local buffer-read-only t)
  (font-lock-mode -1)
  (add-hook 'kill-buffer-hook #'dall-e-mode--kill-buffer-hook nil t)
  (setq-local header-line-format `((:eval (dall-e-header-line))))
  (dall-e-mode-insert-tip))

(defun dall-e-register-instance (index buffer-or-name)
  "Register BUFFER-OR-NAME with INDEX as an instance.

Caution, this will overwrite the existing instance!"
  (ht-set dall-e-instances index (get-buffer-create buffer-or-name))
  (with-current-buffer buffer-or-name
    (dall-e-mode)
    (setq dall-e-instance (cons index (current-buffer)))))

;;;###autoload
(defun dall-e-new ()
  "Run a new instance of DALL-E."
  (interactive)
  (let* ((new-index       (dall-e--new-index))
         (new-buffer-name (format dall-e-buffer-name-format new-index)))
    (when (get-buffer new-buffer-name)
      (user-error "Internal Error: creating instance that already exists"))
    (dall-e-register-instance new-index new-buffer-name)
    (dall-e--pop-to-buffer new-buffer-name)))

;;;###autoload
(defun dall-e ()
  "Start DALL-E with existing instance, else create a new instance."
  (interactive)
  (let ((live-instances  (dall-e--live-instances))
        (shown-instances (dall-e--shown-instances)))
    (cond (shown-instances
           (dall-e--pop-to-buffer (nth 0 shown-instances)))
          (live-instances
           (dall-e--pop-to-buffer (nth 0 live-instances)))
          (t
           (dall-e-new)))))

(provide 'dall-e)
;;; dall-e.el ends here
