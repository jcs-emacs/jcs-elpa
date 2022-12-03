;;; vc-refresh-mode.el --- Refresh vc-state in certain events for better UX  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/vc-refresh-mode
;; Package-Version: 20221203.1554
;; Package-Commit: fc449b70cb94024eca948203efe51d78a4687f24
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: vc

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
;; Refresh vc-state in certain events for better UX
;;

;;; Code:

(require 'vc-hooks)

(defgroup vc-refresh nil
  "Refresh vc-state in certain events for better UX."
  :prefix "vc-refresh-"
  :group 'vc
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/vc-refresh-mode"))

(defcustom vc-refresh-commands
  '( magit-checkout)
  "List of commands/functions to handle."
  :type 'list
  :group 'vc-refresh)

(defun vc-refresh--enable ()
  "Enable function `vc-refresh-mode'."
  (dolist (command vc-refresh-commands)
    (advice-add command :after #'vc-refresh-state)))

(defun vc-refresh--disable ()
  "Disable function `vc-refresh-mode'."
  (dolist (command vc-refresh-commands)
    (advice-remove command #'vc-refresh-state)))

;;;###autoload
(define-minor-mode vc-refresh-mode
  "Minor mode `vc-refresh-mode'."
  :global t
  :require 'vc-refresh-mode
  :group 'eval-mark
  (if vc-refresh-mode (vc-refresh--enable) (vc-refresh--disable)))


(provide 'vc-refresh-mode)
;;; vc-refresh-mode.el ends here

;;; vc-refresh.el ends here
