;;; jcs-echobar.el --- An echo-bar for jcs-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-emacs/jcs-echobar
;; Package-Version: 20230113.1605
;; Package-Commit: b4e5cac855d040e5724ac5e1ad1b18e1c770235a
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (echo-bar "1.0.0") (indent-control "0.1.0") (show-eol "0.1.0") (keycast "1.2.0"))
;; Keywords: faces echo-bar

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
;; An echo-bar for jcs-emacs
;;

;;; Code:

(require 'echo-bar)
(require 'indent-control)
(require 'show-eol)
(require 'keycast)

(defgroup jcs-echobar nil
  "An echo-bar for jcs-emacs."
  :prefix "jcs-echobar-"
  :group 'faces
  :link '(url-link :tag "Github" "https://github.com/jcs-emacs/jcs-echobar"))

(defcustom jcs-echobar-render
  `((:eval (jcs-echobar--render-keycast))
    (:eval (jcs-echobar--render-spaces-tabs-size))
    (:eval (jcs-echobar--render-coding-system))
    (:eval (jcs-echobar--render-eol))
    (:eval (jcs-echobar--render-time)))
  "List of item to render in echo-bar."
  :type 'list
  :group 'jcs-echobar)

(defcustom jcs-echobar-keycast-format "%K%C%R   "
  "The keycast format spec."
  :type 'string
  :group 'jcs-echobar)

;;
;; (@* "Externals" )
;;

(declare-function string-pixel-width "subr-x.el")   ; TODO: remove this after 29.1
(declare-function shr-string-pixel-width "shr.el")  ; TODO: remove this after 29.1

;;
;; (@* "Entry" )
;;

(defvar jcs-echobar--default-function nil
  "Default modeline value to revert back.")

(defun jcs-echobar--enable ()
  "Enable function `jcs-echobar-mode'."
  (progn  ; keycast
    (add-hook 'post-command-hook #'keycast--update t)
    (add-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit t)
    (advice-add 'keycast--update :after #'jcs-echobar--keycast-update))
  (add-hook 'window-size-change-functions #'jcs-echobar--window-resize)
  (jcs-echobar--window-resize)  ; call it manually once
  (setq jcs-echobar--default-function echo-bar-function)
  (setq echo-bar-function #'jcs-echobar-render)
  (echo-bar-mode 1))

(defun jcs-echobar--disable ()
  "Disable function `jcs-echobar-mode'."
  (progn  ; keycast
    (remove-hook 'post-command-hook #'keycast--update)
    (remove-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit)
    (advice-remove 'keycast--update #'jcs-echobar--keycast-update))
  (remove-hook 'window-size-change-functions #'jcs-echobar--window-resize)
  (setq echo-bar-function jcs-echobar--default-function)
  (echo-bar-mode -1))

;;;###autoload
(define-minor-mode jcs-echobar-mode
  "Minor mode `jcs-echobar-mode'."
  :global t
  :require 'jcs-echobar-mode
  :group 'jcs-echobar
  :lighter nil
  (if jcs-echobar-mode (jcs-echobar--enable) (jcs-echobar--disable)))

;;
;; (@* "Util" )
;;

;; TODO: Use function `string-pixel-width' after 29.1
(defun jcs-echobar--string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp #'string-pixel-width)
      (string-pixel-width str)
    (require 'shr)
    (shr-string-pixel-width str)))

(defun jcs-echobar--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (jcs-echobar--string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun jcs-echobar--buffer-spaces-or-tabs ()
  "Check if buffer using spaces or tabs."
  (if (= (how-many "^\t" (point-min) (point-max)) 0) "SPC" "TAB"))

;;
;; (@* "Core" )
;;

(defvar jcs-echobar--render nil)

(defun jcs-echobar--window-resize (&rest _)
  "Window resize hook."
  (setq jcs-echobar--render nil)  ; reset
  (let ((current-width 0))
    (dolist (item jcs-echobar-render)
      (let* ((format (format-mode-line item))
             (width (jcs-echobar--str-len format))
             (new-width (+ current-width width)))
        (when (<= new-width (window-width (minibuffer-window)))
          (setq current-width new-width)
          (push item jcs-echobar--render)))))
  (setq jcs-echobar--render (reverse jcs-echobar--render)))

(defun jcs-echobar-render (&rest _)
  "Render the echo-bar."
  (mapconcat #'format-mode-line jcs-echobar--render ""))

(defun jcs-echobar--keycast-update (&rest _)
  "Exection after `keycast-update' function."
  (when (bound-and-true-p jcs-echobar-mode)
    (echo-bar-update)))

;;
;; (@* "Plugins" )
;;

(defun jcs-echobar--render-spaces-tabs-size ()
  "Render spaces/tabs size."
  (format "%s: %s  "
          (jcs-echobar--buffer-spaces-or-tabs)
          (indent-control-get-indent-level-by-mode)))

(defun jcs-echobar--render-coding-system ()
  "Render buffer coding system."
  (format "%s  " buffer-file-coding-system))

(defun jcs-echobar--render-eol ()
  "Render line-endings."
  (format "%s  " (show-eol-get-eol-mark-by-system)))

(defun jcs-echobar--render-time ()
  "Render time."
  (format-time-string "%b %d, %Y %H:%M:%S"))

(defun jcs-echobar--render-keycast ()
  "Render `keycast'."
  (when (featurep 'keycast)
    (keycast--format jcs-echobar-keycast-format)))

(provide 'jcs-echobar)
;;; jcs-echobar.el ends here
