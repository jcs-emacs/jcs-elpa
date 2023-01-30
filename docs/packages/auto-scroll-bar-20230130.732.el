;;; auto-scroll-bar.el --- Automatically show/hide scroll-bars as needed  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-03-01 03:32:33

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/auto-scroll-bar
;; Package-Version: 20230130.732
;; Package-Commit: 3ddec689d2c6c45dffdb04b7b735154b6a0d4a50
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience scrollbar

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
;; Automatically show/hide scroll-bars as needed.
;;

;;; Code:

(require 'cl-lib)
(require 'scroll-bar)

(defgroup auto-scroll-bar nil
  "Automatically show/hide scroll-bars as needed."
  :prefix "auto-scroll-bar-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/auto-scroll-bar"))

(defcustom auto-scroll-bar-disabled-buffers
  '()
  "List of buffers to disable the scroll bar completely."
  :type 'list
  :group 'auto-scroll-bar)

(defcustom auto-scroll-bar-disabled-major-modes
  '()
  "List of major-mode to disable the scroll bar completely."
  :type 'list
  :group 'auto-scroll-bar)

(defcustom auto-scroll-bar-hide-minibuffer t
  "Non-nil to hide scrollbar in minibuffer."
  :type 'boolean
  :group 'auto-scroll-bar)

(defcustom auto-scroll-bar-horizontal t
  "Set to non-nil to auto show/hide horizontal scroll-bar."
  :type 'boolean
  :group 'auto-scroll-bar)

;;
;; (@* "Externals" )
;;

(declare-function string-pixel-width "subr-x.el")   ; TODO: remove this after 29.1
(declare-function shr-string-pixel-width "shr.el")  ; TODO: remove this after 29.1

;;
;; (@* "Util" )
;;

;; TODO: Use function `string-pixel-width' after 29.1
(defun auto-scroll-bar--string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp #'string-pixel-width)
      (string-pixel-width str)
    (require 'shr)
    (shr-string-pixel-width str)))

(defun auto-scroll-bar--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (auto-scroll-bar--string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defmacro auto-scroll-bar--with-no-redisplay (&rest body)
  "Execute BODY without any redisplay execution."
  (declare (indent 0) (debug t))
  `(let ((inhibit-redisplay t)
         (inhibit-modification-hooks t)
         after-focus-change-function
         buffer-list-update-hook
         display-buffer-alist
         window-configuration-change-hook
         window-scroll-functions
         window-size-change-functions
         window-state-change-hook)
     ,@body))

(defun auto-scroll-bar--window-width ()
  "Calculate inner window width."
  (window-max-chars-per-line))

(defun auto-scroll-bar--window-height ()
  "Calculate inner window height."
  (let ((height (window-height)))
    (when horizontal-scroll-bar
      (cl-decf height (scroll-bar-lines)))
    height))

;;
;; (@* "Core" )
;;

(defun auto-scroll-bar--show-v-p ()
  "Return non-nil if we should show the vertical scroll-bar."
  (and vertical-scroll-bar
       (not (and (= (point-min) (window-start))
                 (= (point-max) (window-end nil t))))))

(defun auto-scroll-bar--show-h-p ()
  "Return non-nil if we should show the horizontal scroll-bar."
  (and
   horizontal-scroll-bar
   truncate-lines
   (or
    ;; (1) When window not align to the left!
    (let ((w-hscroll (max (- (window-hscroll) hscroll-step) 0)))
      (and (not (zerop w-hscroll))
           (<= w-hscroll (current-column))))
    ;; (2) When at least one line exceeds the current window width
    (save-excursion
      (move-to-window-line 0)
      (let* ((win-w (auto-scroll-bar--window-width))
             (win-h (auto-scroll-bar--window-height))
             (count 0) (target win-h) break)
        (while (and (not (eobp)) (< count target) (not break))
          (let* ((line-str (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
                 (line-len (auto-scroll-bar--str-len line-str)))
            (if (< win-w line-len)
                (setq break t)
              (forward-line 1)
              (cl-incf count))))
        break)))))

(defun auto-scroll-bar--disabled-p ()
  "Return non-nil if scroll-bars should be ignored."
  (or (member (buffer-name) auto-scroll-bar-disabled-buffers)
      (member major-mode auto-scroll-bar-disabled-major-modes)))

(defun auto-scroll-bar--toggle-p (win show-v show-h)
  "Return non-nil if we should call function `set-window-scroll-bars'.

See function `auto-scroll-bar--update' description for arguments WIN, SHOW-V,
and SHOW-H."
  (let* ((bars (window-scroll-bars win))
         (shown-v (nth 2 bars))
         (shown-h (nth 5 bars)))
    (or (not (eq shown-v show-v)) (not (eq shown-h show-h)))))

(defun auto-scroll-bar--update (win show-v show-h &optional persistent)
  "Update scrollbar WIN, SHOW-V, SHOW-H, PERSISTENT."
  (when (auto-scroll-bar--toggle-p win show-v show-h)
    (set-window-scroll-bars win nil show-v nil show-h persistent)
    (save-window-excursion (ignore-errors (window-resize win 0)))))  ; refresh

(defun auto-scroll-bar--show-hide (win)
  "Show/Hide scroll-bar for WIN."
  (with-selected-window win
    (if (auto-scroll-bar--disabled-p)
        (auto-scroll-bar--update win nil nil)
      (let ((show-v (auto-scroll-bar--show-v-p))
            (show-h (auto-scroll-bar--show-h-p)))
        (auto-scroll-bar--update win show-v show-h)))))

(defun auto-scroll-bar--hide-minibuffer (&optional frame)
  "Hide minibuffer when variable `auto-scroll-bar-hide-minibuffer' is enabled.

Optional argument FRAME is used to select frame's minibuffer."
  (when auto-scroll-bar-hide-minibuffer
    (auto-scroll-bar--update (minibuffer-window frame) nil nil t)))

(defun auto-scroll-bar--size-change (&optional frame &rest _)
  "Show/Hide all visible windows in FRAME."
  (auto-scroll-bar--with-no-redisplay
    (dolist (win (window-list frame)) (auto-scroll-bar--show-hide win))
    (auto-scroll-bar--hide-minibuffer frame)))

(defun auto-scroll-bar--scroll (&optional window &rest _)
  "Show/Hide scroll-bar on WINDOW."
  (auto-scroll-bar--with-no-redisplay
    (when (windowp window) (auto-scroll-bar--show-hide window))))

(defun auto-scroll-bar--post-command (&rest _)
  "Hook for post-command."
  (dolist (window (get-buffer-window-list))
    (if (equal (minibuffer-window) window) (auto-scroll-bar--hide-minibuffer)
      (auto-scroll-bar--scroll window))))

(defun auto-scroll-bar--enable ()
  "Enable function `auto-scroll-bar-mode'."
  (cond ((display-graphic-p)
         (add-hook 'window-size-change-functions #'auto-scroll-bar--size-change)
         (add-hook 'window-scroll-functions #'auto-scroll-bar--scroll)
         (add-hook 'post-command-hook #'auto-scroll-bar--post-command)
         (toggle-scroll-bar 1)
         (when auto-scroll-bar-horizontal (toggle-horizontal-scroll-bar 1))
         (auto-scroll-bar--size-change))  ; execute once
        (t (auto-scroll-bar-mode -1))))

(defun auto-scroll-bar--disable ()
  "Disable function `auto-scroll-bar-mode'."
  (remove-hook 'window-size-change-functions #'auto-scroll-bar--size-change)
  (remove-hook 'window-scroll-functions #'auto-scroll-bar--scroll)
  (remove-hook 'post-command-hook #'auto-scroll-bar--post-command)
  (toggle-scroll-bar -1)
  (toggle-horizontal-scroll-bar -1))

;;;###autoload
(define-minor-mode auto-scroll-bar-mode
  "Minor mode `auto-scroll-bar-mode'."
  :global t
  :require 'auto-scroll-bar-mode
  :group 'auto-scroll-bar
  (if auto-scroll-bar-mode (auto-scroll-bar--enable) (auto-scroll-bar--disable)))

(provide 'auto-scroll-bar)
;;; auto-scroll-bar.el ends here
