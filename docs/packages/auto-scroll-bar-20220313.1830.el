;;; auto-scroll-bar.el --- Automatically show/hide scroll-bars as needed  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-03-01 03:32:33

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Automatically show/hide scroll-bars as needed.
;; Keyword: scrollbar
;; Version: 0.1.0
;; Package-Version: 20220313.1830
;; Package-Commit: e9fb68bec7526b6549f2817ee267203e69e04dd7
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/jcs-elpa/auto-scroll-bar

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
  "List of buffers to disable the scroll bar completely."
  :type 'boolean
  :group 'auto-scroll-bar)

(defcustom auto-scroll-bar-horizontal t
  "Set to non-nil to auto show/hide horizontal scroll-bar."
  :type 'boolean
  :group 'auto-scroll-bar)

;;
;; (@* "Util" )
;;

(defmacro auto-scroll-bar--with-no-redisplay (&rest body)
  "Execute BODY without any redisplay execution."
  (declare (indent 0) (debug t))
  `(let ((inhibit-redisplay t)
         (inhibit-modification-hooks t)
         (inhibit-point-motion-hooks t)
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
  (let ((width (window-width)))
    (when (bound-and-true-p display-line-numbers-mode)
      (cl-decf width (line-number-display-width)))
    (when vertical-scroll-bar
      (cl-decf width (scroll-bar-columns (get-scroll-bar-mode))))
    width))

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
   (save-excursion
     (move-to-window-line 0)
     (let ((count 0) (target (auto-scroll-bar--window-height)) break)
       (while (and (not (eobp)) (< count target) (not break))
         (if (< (auto-scroll-bar--window-width) (- (line-end-position) (line-beginning-position)))
             (setq break t)
           (forward-line 1)
           (cl-incf count)))
       break))))

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
    (save-window-excursion (ignore-errors (enlarge-window 1)))))  ; refresh

(defun auto-scroll-bar--show-hide (win)
  "Show/Hide scroll-bar for WIN."
  (with-selected-window win
    (if (auto-scroll-bar--disabled-p)
        (auto-scroll-bar--update win nil nil)
      (let ((show-v (auto-scroll-bar--show-v-p))
            (show-h (auto-scroll-bar--show-h-p)))
        (auto-scroll-bar--update win show-v show-h)))))

(defun auto-scroll-bar--hide-minibuffer ()
  "Hide minibuffer when variable `auto-scroll-bar-hide-minibuffer' is enabled."
  (when auto-scroll-bar-hide-minibuffer
    (auto-scroll-bar--update (minibuffer-window) nil nil t)))

(defun auto-scroll-bar--size-change (&rest _)
  "Show/Hide all visible windows."
  (auto-scroll-bar--with-no-redisplay
    (dolist (win (window-list)) (auto-scroll-bar--show-hide win))
    (auto-scroll-bar--hide-minibuffer)))

(defun auto-scroll-bar--scroll (&optional window &rest _)
  "Show/Hide scroll-bar on WINDOW."
  (auto-scroll-bar--with-no-redisplay
    (when (windowp window) (auto-scroll-bar--show-hide window))))

(defun auto-scroll-bar--enable ()
  "Enable function `auto-scroll-bar-mode'."
  (add-hook 'window-size-change-functions #'auto-scroll-bar--size-change)
  (add-hook 'window-scroll-functions #'auto-scroll-bar--scroll)
  (toggle-scroll-bar 1)
  (when auto-scroll-bar-horizontal (toggle-horizontal-scroll-bar 1))
  (auto-scroll-bar--size-change))  ; execute once

(defun auto-scroll-bar--disable ()
  "Disable function `auto-scroll-bar-mode'."
  (remove-hook 'window-size-change-functions #'auto-scroll-bar--size-change)
  (remove-hook 'window-scroll-functions #'auto-scroll-bar--scroll)
  (toggle-scroll-bar -1)
  (toggle-horizontal-scroll-bar -1))

;;;###autoload
(define-minor-mode auto-scroll-bar-mode
  "Minor mode 'auto-scroll-bar-mode'."
  :global t
  :require 'auto-scroll-bar-mode
  :group 'auto-scroll-bar
  (if auto-scroll-bar-mode (auto-scroll-bar--enable) (auto-scroll-bar--disable)))

(provide 'auto-scroll-bar)
;;; auto-scroll-bar.el ends here
