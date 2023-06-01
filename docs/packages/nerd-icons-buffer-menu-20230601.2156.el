;;; nerd-icons-buffer-menu.el --- Display nerd icons in `buffer-menu'  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/nerd-icons-buffer-menu
;; Package-Version: 20230601.2156
;; Package-Commit: 2733f1077a7b5dd1240e7b5beab512f79b5a52fe
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (nerd-icons "0.0.1") (noflet "0.0.15") (msgu "0.1.0"))
;; Keywords: frames

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
;; Display nerd icons in `buffer-menu'.
;;

;;; Code:

(require 'msgu)
(require 'nerd-icons)
(require 'noflet)

(defgroup nerd-icons-buffer-menu nil
  "Display nerd icons in `buffer-menu'."
  :prefix "nerd-icons-buffer-menu-"
  :group 'frames
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/nerd-icons-buffer-menu"))

;;
;;; Entry

(defun nerd-icons-buffer-menu--enable ()
  "Enable `nerd-icons-buffer-menu-mode'."
  (advice-add 'list-buffers--refresh :around #'nerd-icons-buffer-menu--refresh)
  (msgu-silent (load-library "buff-menu.el")))  ; need to reload

(defun nerd-icons-buffer-menu--disable ()
  "Disable `nerd-icons-buffer-menu-mode'."
  (advice-remove 'list-buffers--refresh #'nerd-icons-buffer-menu--refresh))

;;;###autoload
(define-minor-mode nerd-icons-buffer-menu-mode
  "Minor mode `nerd-icons-buffer-menu-mode'."
  :global t
  :require 'nerd-icons-buffer-menu
  :group 'nerd-icons-buffer-menu
  (if nerd-icons-buffer-menu-mode (nerd-icons-buffer-menu--enable)
    (nerd-icons-buffer-menu--disable)))

;;
;;; Core

(defun nerd-icons-buffer-menu--refresh (func &rest args)
  "Execute around function `list-buffers--refresh'."
  (if (not nerd-icons-buffer-menu-mode)
      (apply func args)
    (let ((original-function (symbol-function 'format-mode-line)))
      (noflet
        ((format-mode-line
          (format &optional face window buffer &rest _)
          (let ((original-value (funcall original-function format face window buffer)))
            (if (equal format mode-name)
                (let ((icon (let* ((icon (nerd-icons-icon-for-buffer))
                                   (icon (if (or (null icon) (symbolp icon))
                                             (nerd-icons-faicon "nf-fa-file_o")
                                           icon)))
                              (if (and icon
                                       (char-displayable-p (string-to-char icon)))
                                  (concat icon " ")
                                ""))))
                  (concat icon original-value))
              original-value))))
        (apply func args)))))

(provide 'nerd-icons-buffer-menu)
;;; nerd-icons-buffer-menu.el ends here
