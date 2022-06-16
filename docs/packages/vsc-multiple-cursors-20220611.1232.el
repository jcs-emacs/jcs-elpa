;;; vsc-multiple-cursors.el --- multiple-curosrs integration behave like VSCode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/vsc-multiple-cursors
;; Package-Version: 20220611.1232
;; Package-Commit: f097bfd708c2b92bed0af5e6965bc1b2781a2efa
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (multiple-cursors "1.4.0"))
;; Keywords: vscode vsc multiple-cursors

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
;; multiple-curosrs integration behave like VSCode.
;;

;;; Code:

(require 'multiple-cursors)

(defgroup vsc-multiple-cursors nil
  "VSCode integration multiple-curosrs."
  :prefix "vsc-multiple-cursors-"
  :group 'convenience
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/vsc-multiple-cursors"))

(defcustom vsc-multiple-cursors-similarity 20
  "The standard similarity, the lower require more precision."
  :type 'number
  :group 'vsc-multiple-cursors)

;;;###autoload
(defun vsc-multiple-cursors-mark-previous-like-this-line ()
  "Smart marking previous line."
  (interactive)
  (require 'multiple-cursors)
  (let ((before-unmark-cur-cnt (mc/num-cursors))
        (unmark-do (ignore-errors (call-interactively #'mc/unmark-next-like-this))))
    (unless unmark-do
      (unless (> before-unmark-cur-cnt (mc/num-cursors))
        (call-interactively #'mc/mark-previous-like-this)))))

;;;###autoload
(defun vsc-multiple-cursors-mark-next-like-this-line ()
  "Smart marking next line."
  (interactive)
  (let ((before-unmark-cur-cnt (mc/num-cursors))
        (unmark-do (ignore-errors (call-interactively #'mc/unmark-previous-like-this))))
    (unless unmark-do
      (unless (> before-unmark-cur-cnt (mc/num-cursors))
        (call-interactively #'mc/mark-next-like-this)))))

(defun vsc-multiple-cursors--furthest-cursor-before-point ()
  "Goto the furthest cursor before point."
  (when (mc/furthest-cursor-before-point) (goto-char (overlay-end (mc/furthest-cursor-before-point)))))

(defun vsc-multiple-cursors--furthest-cursor-after-point ()
  "Goto furthest cursor after point."
  (when (mc/furthest-cursor-after-point) (goto-char (overlay-end (mc/furthest-cursor-after-point)))))

;;;###autoload
(defun vsc-multiple-cursors-mark-previous-similar-this-line (&optional sdl)
  "Mark previous line similar to this line depends on string distance level (SDL)."
  (interactive)
  (unless sdl (setq sdl vsc-multiple-cursors-similarity))
  (save-excursion
    (let ((cur-line (thing-at-point 'line)) (cur-col (current-column))
          sim-line break)
      (vsc-multiple-cursors--furthest-cursor-before-point)
      (forward-line -1)
      (while (and (not break) (not (= (line-number-at-pos (point)) (line-number-at-pos (point-min)))))
        (setq sim-line (thing-at-point 'line))
        (when (and (< (string-distance sim-line cur-line) sdl)
                   (or (and (not (string= "\n" sim-line)) (not (string= "\n" cur-line)))
                       (and (string= "\n" sim-line) (string= "\n" cur-line))))
          (move-to-column cur-col)
          (mc/create-fake-cursor-at-point)
          (setq break t))
        (forward-line -1))
      (unless break (user-error "[INFO] no previous similar match"))))
  (mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun vsc-multiple-cursors-mark-next-similar-this-line (&optional sdl)
  "Mark next line similar to this line depends on string distance level (SDL)."
  (interactive)
  (unless sdl (setq sdl vsc-multiple-cursors-similarity))
  (save-excursion
    (let ((cur-line (thing-at-point 'line)) (cur-col (current-column))
          sim-line break)
      (vsc-multiple-cursors--furthest-cursor-after-point)
      (forward-line 1)
      (while (and (not break) (not (= (line-number-at-pos (point)) (line-number-at-pos (point-max)))))
        (setq sim-line (thing-at-point 'line))
        (when (and (< (string-distance sim-line cur-line) sdl)
                   (or (and (not (string= "\n" sim-line)) (not (string= "\n" cur-line)))
                       (and (string= "\n" sim-line) (string= "\n" cur-line))))
          (move-to-column cur-col)
          (mc/create-fake-cursor-at-point)
          (setq break t))
        (forward-line 1))
      (unless break (user-error "[INFO] no next similar match"))))
  (mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun vsc-multiple-cursors-inc-similarity ()
  "Increase the string distance level by 1."
  (interactive)
  (setq vsc-multiple-cursors-similarity (1+ vsc-multiple-cursors-similarity))
  (message "[INFO] MC similarity: %s" vsc-multiple-cursors-similarity))

;;;###autoload
(defun vsc-multiple-cursors-dec-similarity ()
  "Decrease the string distance level by 1."
  (interactive)
  (setq vsc-multiple-cursors-similarity (1- vsc-multiple-cursors-similarity))
  (message "[INFO] MC similarity: %s" vsc-multiple-cursors-similarity))

(provide 'vsc-multiple-cursors)
;;; vsc-multiple-cursors.el ends here
