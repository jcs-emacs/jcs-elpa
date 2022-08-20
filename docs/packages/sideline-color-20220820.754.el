;;; sideline-color.el --- Show color information with sideline  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-sideline/sideline-color
;; Package-Version: 20220820.754
;; Package-Commit: fc1a447cc6e02d39026d0ec8d669d1542d1792bb
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (sideline "0.1.0"))
;; Keywords: convenience color

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
;; This package allows display color information with sideline.
;;
;; 1) Add sideline-color to sideline backends list,
;;
;;   (setq sideline-backends-right '(sideline-color))
;;
;; 2) Then enable sideline-mode in the target buffer,
;;
;;   M-x sideline-mode
;;

;;; Code:

(require 'faces)

(require 'sideline)

(defgroup sideline-color nil
  "Show color information with sideline."
  :prefix "sideline-color-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-sideline/sideline-color"))

(defcustom sideline-color-text "▓▒░"
  "String to display color."
  :type 'string
  :group 'sideline-color)

(defun sideline-color--thing-at-point (thing &optional no-properties)
  "Same with function `thing-at-point' but respect region.

Arguments THING and NO-PROPERTIES have same effect to function `thing-at-point'."
  (if (use-region-p)
      (if no-properties
          (buffer-substring-no-properties (region-beginning) (region-end))
        (buffer-substring (region-beginning) (region-end)))
    (thing-at-point thing no-properties)))

;;;###autoload
(defun sideline-color (command)
  "Backend for sideline.

Argument COMMAND is required in sideline backend."
  (cl-case command
    (`candidates (cons :async #'sideline-color--show))))

(defun sideline-color--show (callback &rest _)
  "Execute CALLBACK to display with sideline."
  (when-let* ((text (sideline-color--thing-at-point 'symbol t))
              (color (color-values text))
              (display (concat (propertize sideline-color-text 'font-lock-face `(:foreground ,text)) " " text)))
    (funcall callback (list display))))

(provide 'sideline-color)
;;; sideline-color.el ends here
