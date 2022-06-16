;;; preview-it.el --- Preview anything at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Shen, Jen-Chieh
;; Created date 2020-10-13 12:46:22

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/preview-it
;; Package-Version: 20220616.1941
;; Package-Commit: b37d4b823e69ecea5b7de59e62a5017fe54fd655
;; Version: 1.1.0
;; Package-Requires: ((emacs "26.1") (posframe "1.1.7") (request "0.3.0") (gh-md "0.1.1"))
;; Keywords: preview image path file

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
;; This package is to attempt to preview any data as possible.  Data can
;; refers to `file', `image', or `url'.
;;
;; You may enabled this package by doing the following execution.
;;
;;   `(preview-it-mode 1)`
;;
;; Or you can either enabled it globally by doing this.
;;
;;   `(global-preview-it-mode 1)`
;;

;;; Code:

(require 'cl-lib)
(require 'ffap)
(require 'shr)
(require 'image-mode)

(require 'posframe)
(require 'request)
(require 'gh-md)

(defgroup preview-it nil
  "Preview anything at point."
  :prefix "preview-it-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/preview-it"))

(defface preview-it-background
  '((((background light)) :background "#E9EAED")
    (t :background "#2A2D38"))
  "Background color of the preview buffer."
  :group 'preview-it)

(defcustom preview-it-delay 0.4
  "Seconds delay to show preview."
  :type 'float
  :group 'preview-it)

(defcustom preview-it-render-md nil
  "Set to non-nil, render markdown file."
  :type 'boolean
  :group 'preview-it)

(defconst preview-it--buffer-name "*preview-it*"
  "Name of the preview buffer.")

(defvar preview-it--image-extensions
  '("jpg" "png" "jpeg" "gif" "bmp" "svg")
  "List of image extensions.")

(defvar preview-it--timer nil
  "Display timer after hovering.")

(defvar preview-it--url-request nil
  "Request when browsing URL.")

;;
;; (@* "Entry" )
;;

(defun preview-it--enable ()
  "Enable 'preview-it-mode'."
  (add-hook 'pre-command-hook #'preview-it--stop-preview nil t)
  (add-hook 'post-command-hook #'preview-it--start-preview nil t))

(defun preview-it--disable ()
  "Disable 'preview-it-mode'."
  (remove-hook 'pre-command-hook #'preview-it--stop-preview t)
  (remove-hook 'post-command-hook #'preview-it--start-preview t)
  (preview-it--stop-preview))

;;;###autoload
(define-minor-mode preview-it-mode
  "Minor mode 'preview-it-mode'."
  :require 'preview-it
  :group 'preview-it
  (if preview-it-mode (preview-it--enable) (preview-it--disable)))

(defun preview-it--turn-on-preview-it-mode ()
  "Turn on the 'preview-it-mode'."
  (preview-it-mode 1))

;;;###autoload
(define-globalized-minor-mode global-preview-it-mode
  preview-it-mode preview-it--turn-on-preview-it-mode
  :require 'preview-it)

;;
;; (@* "Util" )
;;

(defmacro preview-it--mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let (message-log-max)
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) ,@body))))

(defun preview-it--is-contain-list-string-regexp (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST."
  (cl-some (lambda (lb-sub-str) (string-match-p lb-sub-str in-str)) in-list))

(defun preview-it--kill-timer (timer)
  "Safe way to kill TIMER."
  (when (timerp timer) (cancel-timer timer)))

(defun preview-it--text-file-p (filename)
  "Return non-nil if FILENAME is a text file; not binary file."
  (let ((inhibit-message t)message-log-max)
    (with-current-buffer (find-file-noselect filename :no-warn)
      (prog1 (not (eq buffer-file-coding-system 'no-conversion))
        (kill-buffer)))))

(defun preview-it--is-file-p (path)
  "Check if PATH a file path."
  (and (file-exists-p path) (not (file-directory-p path))))

(defmacro preview-it--with-preview-buffer (&rest body)
  "Execute BODY inside preview buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer (get-buffer-create preview-it--buffer-name)
     (let ((inhibit-read-only t)) ,@body)))

;;
;; (@* "Url" )
;;

(defun preview-it--reset-request ()
  "Reset URL request."
  (when preview-it--url-request (request-abort preview-it--url-request)))

(cl-defun preview-it--receive-data (&key data &allow-other-keys)
  "Callback after receiving URL DATA."
  (preview-it--with-preview-buffer
    (erase-buffer)
    (insert data)
    ;; TODO: The check here is fragile. Need a much dedicated method to
    ;; detect html string.
    (when (string-match-p "<!doctype html>" data)
      (shr-render-region (point-min) (point-max))))
  (preview-it--show))

;;
;; (@* "Core" )
;;

(defun preview-it--content-empty-p ()
  "Return non-nil if content is empty."
  (string-empty-p (preview-it--with-preview-buffer (buffer-string))))

(defun preview-it--save-window-layout (fnc &rest args)
  "Advice execute around FNC, ARGS."
  (save-window-excursion (apply fnc args))
  (advice-remove 'gh-md--callback #'preview-it--save-window-layout))  ; Remove self

(defun preview-it--render-file (path)
  "Render local file PATH."
  (let ((ext (file-name-extension path)))
    (cond
     ((member ext preview-it--image-extensions)
      (preview-it--with-preview-buffer
        (ignore-errors (insert-image-file path))
        (image-mode)))
     ((and preview-it-render-md (member ext '("md" "markdown")))
      (advice-add 'gh-md--callback :around #'preview-it--save-window-layout)
      (let ((gh-md-buffer-name preview-it--buffer-name))
        (preview-it--with-preview-buffer
          (ignore-errors (insert-file-contents path))
          (gh-md-render-buffer))))
     ;; TODO: This method is very slow, need to find other replacement.
     ((preview-it--text-file-p path)
      (setq path (expand-file-name path))
      (preview-it--with-preview-buffer
        (insert-file-contents path)
        (let ((buffer-file-name path)) (delay-mode-hooks (set-auto-mode)))
        (ignore-errors (font-lock-ensure)))))))

(defun preview-it--render-url (path)
  "Render http PATH."
  (setq preview-it--url-request
        (request
          path
          :type "GET"
          :parser 'buffer-string
          :success 'preview-it--receive-data)))

;;;###autoload
(defun preview-it ()
  "Preview thing at point."
  (interactive)
  (when-let ((path (ffap-guesser)))
    (preview-it--with-preview-buffer (erase-buffer))
    (cond
     ((preview-it--is-file-p path)  ; file
      (preview-it--mute-apply (preview-it--render-file path)))
     ;; TODO: Not sure if there are other cases.
     ((string-match-p "http[s]*://" path)  ; request
      (preview-it--render-url path)))
    (unless (preview-it--content-empty-p)
      (preview-it--show)
      (add-hook 'post-command-hook #'preview-it--post))))

(defun preview-it--show ()
  "Show preview frame."
  (posframe-show preview-it--buffer-name :position (point)
                 :border-width 10
                 :background-color (face-background 'preview-it-background)))

(defun preview-it--next ()
  "Hide tooltip after first post command."
  (preview-it--stop-preview)
  (remove-hook 'post-command-hook #'preview-it--next))

(defun preview-it--post ()
  "Register for next post command."
  (add-hook 'post-command-hook #'preview-it--next)
  (remove-hook 'post-command-hook #'preview-it--post))

(defun preview-it--start-preview ()
  "Trigger to start previewing."
  (preview-it--stop-preview)
  (preview-it--reset-request)
  (preview-it--kill-timer preview-it--timer)
  (setq preview-it--timer (run-with-idle-timer preview-it-delay nil #'preview-it)))

(defun preview-it--stop-preview ()
  "Trigger to stop previewing."
  (posframe-hide preview-it--buffer-name))

(provide 'preview-it)
;;; preview-it.el ends here
