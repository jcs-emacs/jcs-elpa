;;; repos-window.el --- Reposition window if when needed  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/repos-window
;; Package-Version: 20221103.2116
;; Package-Commit: 0b04112e24513f401041601fe941d59d7cd740d5
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
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
;; Reposition window if when needed
;;

;;; Code:

(defgroup repos-window nil
  "Reposition window if when needed."
  :prefix "repos-window-"
  :group 'convenience
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/repos-window"))

(defcustom repos-window-commands
  '()
  "List of commands to reposition."
  :type 'list
  :group 'repos-window)

;;;###autoload
(defun repos-window-top-bottom (type)
  "Recenter the window by TYPE."
  (let ((recenter-positions `(,type))) (ignore-errors (recenter-top-bottom))))

;;;###autoload
(defun repos-window-middle-after (&rest _)
  "Middle window."
  (repos-window-top-bottom 'middle))

(defun repos-window--enable ()
  "Enable function `repos-window-mode'."
  (dolist (command repos-window-commands)
    (advice-add command :after #'repos-window-middle-after)))

(defun repos-window--disable ()
  "Disable function `repos-window-mode'."
  (dolist (command repos-window-commands)
    (advice-remove command #'repos-window-middle-after)))

;;;###autoload
(define-minor-mode repos-window-mode
  "Minor mode `repos-window-mode'."
  :global t
  :require 'repos-window-mode
  :group 'repos-window
  (if repos-window-mode (repos-window--enable) (repos-window--disable)))

(provide 'repos-window)
;;; repos-window.el ends here
