;;; jcs-frametitle.el --- A frame title for jcs-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-emacs/jcs-frametitle
;; Package-Version: 20230106.1653
;; Package-Commit: cc4a6d287e9dc1cb141eda6375010b5aae00ba85
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces frame-title

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
;; A frame title for jcs-emacs.
;;

;;; Code:

(defgroup jcs-frametitle nil
  "An frame title for jcs-emacs."
  :prefix "jcs-frametitle-"
  :group 'faces
  :link '(url-link :tag "Github" "https://github.com/jcs-emacs/jcs-frametitle"))

(defcustom jcs-frametitle-render
  `((:eval (jcs-frametitle--render-invocation))
    (:eval (jcs-frametitle--render-username-host))
    (:eval (jcs-frametitle--render-buffer-name)))
  "List of item to render in frame title."
  :type 'list
  :group 'jcs-frametitle)

(defvar jcs-frametitle--render nil)

;;
;; (@* "Externals" )
;;

(declare-function string-pixel-width "subr-x.el")   ; TODO: remove this after 29.1
(declare-function shr-string-pixel-width "shr.el")  ; TODO: remove this after 29.1

;;
;; (@* "Entry" )
;;

(defvar jcs-frametitle--default-format nil
  "Default modeline value to revert back.")

(defun jcs-frametitle--enable ()
  "Enable function `jcs-frametitle-mode'."
  (add-hook 'window-size-change-functions #'jcs-frametitle--window-resize)
  (jcs-frametitle--window-resize)  ; call it manually once
  (setq jcs-frametitle--default-format frame-title-format)
  (setq-default frame-title-format '((:eval (jcs-frametitle-render)))))

(defun jcs-frametitle--disable ()
  "Disable function `jcs-frametitle-mode'."
  (remove-hook 'window-size-change-functions #'jcs-frametitle--window-resize)
  (setq-default frame-title-format jcs-frametitle--default-format))

;;;###autoload
(define-minor-mode jcs-frametitle-mode
  "Minor mode `jcs-frametitle-mode'."
  :global t
  :require 'jcs-frametitle-mode
  :group 'jcs-frametitle
  :lighter nil
  (if jcs-frametitle-mode (jcs-frametitle--enable) (jcs-frametitle--disable)))

;;
;; (@* "Util" )
;;

;; TODO: Use function `string-pixel-width' after 29.1
(defun jcs-frametitle--string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp #'string-pixel-width)
      (string-pixel-width str)
    (require 'shr)
    (shr-string-pixel-width str)))

(defun jcs-frametitle--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (jcs-frametitle--string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

;;
;; (@* "Core" )
;;

(defun jcs-frametitle--window-resize (&rest _)
  "Window resize hook."
  (setq jcs-frametitle--render nil)  ; reset
  (let ((current-width 0))
    (dolist (item jcs-frametitle-render)
      (let* ((format (format-mode-line item))
             (width (jcs-frametitle--str-len format))
             (new-width (+ current-width width)))
        (when (<= new-width (window-width (minibuffer-window)))
          (setq current-width new-width)
          (push item jcs-frametitle--render)))))
  (setq jcs-frametitle--render (reverse jcs-frametitle--render)))

(defun jcs-frametitle-render ()
  "Render the frame title."
  (mapconcat #'format-mode-line jcs-frametitle--render ""))

;;
;; (@* "Plugins" )
;;

(defun jcs-frametitle--render-invocation ()
  "Render invocation."
  (concat invocation-name " - "))

(defun jcs-frametitle--render-username-host ()
  "Render username and host."
  (format "%s@%s: " user-real-login-name (system-name)))

(defun jcs-frametitle--render-buffer-name ()
  "Render buffer name."
  (concat  (if (and buffer-file-name (buffer-modified-p)) "*" "")
           (if buffer-file-name "%f" "%b")))

(provide 'jcs-frametitle)
;;; jcs-frametitle.el ends here
