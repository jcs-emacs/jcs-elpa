;;; undo-tree-vf.el --- visualizer follow mode for undo-tree  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/undo-tree-vf
;; Package-Version: 20221003.1207
;; Package-Commit: bd69e50e98107aab0fde54061e561347b2e7d9e7
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (undo-tree "0.8.2") (fill-page "0.3.7"))
;; Keywords: convenience

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
;; visualizer follow mode for undo-tree.
;;

;;; Code:

(require 'undo-tree)
(require 'fill-page)

(defgroup undo-tree-vf nil
  "visualizer follow mode for undo-tree."
  :prefix "undo-tree-vf-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/undo-tree-vf"))

(defcustom undo-tree-vf-fallback-undo #'undo
  "Fallback undo key."
  :type 'function
  :group 'undo-tree-vf)

(defcustom undo-tree-vf-fallback-redo #'undo  ; undo/redo are the same command
  "Fallback redo key."
  :type 'function
  :group 'undo-tree-vf)
;;
;; (@* "Util" )
;;

(defun undo-tree-vf--recenter-top-bottom ()
  "Recenter to center."
  (let ((recenter-positions '(middle)))
    (ignore-errors (recenter-top-bottom))))

(defmacro undo-tree-vf--if-buffer-window (buffer-or-name then &rest else)
  "Execute THEN in window BUFFER-OR-NAME; otherwise ELSE will be executed."
  (declare (indent 2) (debug t))
  `(if-let ((win (ignore-errors (get-buffer-window-list ,buffer-or-name))))
       (with-selected-window (nth 0 win) ,then)
     ,@else))

(defmacro undo-tree-vf--setup (arg &rest body)
  "Set up BODY for undo/redo.

If `undo-tree-mode' is not valid, we call undo/redo function according to ARG"
  (declare (indent 1))
  `(if (not undo-tree-mode)
       (call-interactively (if ,arg undo-tree-vf-fallback-undo
                             undo-tree-vf-fallback-redo))
     ,@body))

;;
;; (@* "Entry" )
;;

(defun undo-tree-vf-mode--enable ()
  "Enable function `undo-tree-vf-mode'."
  (advice-add #'save-buffer :after #'undo-tree-kill-visualizer)
  (advice-add #'kill-this-buffer :after #'undo-tree-kill-visualizer))

(defun undo-tree-vf-mode--disable ()
  "Disable function `undo-tree-vf-mode'."
  (advice-remove #'save-buffer #'undo-tree-kill-visualizer)
  (advice-remove #'kill-this-buffer  #'undo-tree-kill-visualizer))

;;;###autoload
(define-minor-mode undo-tree-vf-mode
  "Minor mode `undo-tree-vf-mode'."
  :global t
  :require 'undo-tree-vf-mode
  :group 'undo-tree-vf
  (if undo-tree-vf-mode (undo-tree-vf-mode--enable) (undo-tree-vf-mode--disable)))

;;
;; (@* "Core" )
;;

(defun undo-tree-vf--visualize ()
  "Call `undo-tree-visualize' only in window that has higher height."
  (save-window-excursion (undo-tree-visualize))
  (with-selected-window (get-largest-window nil nil t)
    (switch-to-buffer undo-tree-visualizer-buffer-name)
    (undo-tree-vf--recenter-top-bottom)
    (fill-page-if-unfill)))

(defun undo-tree-vf--undo-or-redo (ud)
  "Do undo or redo base on UD.

If UD is non-nil, do undo.  If UD is nil, do redo."
  (undo-tree-vf--setup ud
    ;; NOTE: If we do jumped to the `undo-tree-visualizer-buffer-name'
    ;; buffer, then we use `undo-tree-visualize-redo' instead of
    ;; `undo-tree-redo'. Because directly called `undo-tree-visualize-redo'
    ;; key is way faster than `undo-tree-redo' key.
    (undo-tree-vf--if-buffer-window undo-tree-visualizer-buffer-name
        (if ud (undo-tree-visualize-undo) (undo-tree-visualize-redo))
      (if ud (undo-tree-undo) (undo-tree-redo))
      (undo-tree-vf--visualize))))

;;;###autoload
(defun undo-tree-vf-undo ()
  "Undo with visualizer."
  (interactive)
  (undo-tree-vf--undo-or-redo t))

;;;###autoload
(defun undo-tree-vf-redo ()
  "Redo with visualizer."
  (interactive)
  (undo-tree-vf--undo-or-redo nil))

(provide 'undo-tree-vf)
;;; undo-tree-vf.el ends here
