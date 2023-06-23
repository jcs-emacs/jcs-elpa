;;; eval-mark.el --- Evaluate then deactive mark  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/eval-mark
;; Package-Version: 20221119.1128
;; Package-Commit: 6749ce3326529dd503ee5a5170516c2aad84b55b
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: lisp

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
;; Evaluate then deactive mark
;;

;;; Code:

(defgroup eval-mark nil
  "Evaluate then deactive mark."
  :prefix "eval-mark-"
  :group 'convenience
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/eval-mark"))

(defcustom eval-mark-commands-before
  '( keyboard-quit top-level)
  "List of commands to handle."
  :type 'list
  :group 'eval-mark)

(defcustom eval-mark-commands-after
  '( eval-buffer eval-defun eval-region)
  "List of commands to handle."
  :type 'list
  :group 'eval-mark)

(defun eval-mark--deactivate-mark (&rest _) "Deactive mark." (deactivate-mark))

(defun eval-mark--enable ()
  "Enable function `eval-mark-mode'."
  (dolist (command eval-mark-commands-before)
    (advice-add command :before #'eval-mark--deactivate-mark))
  (dolist (command eval-mark-commands-after)
    (advice-add command :after #'eval-mark--deactivate-mark)))

(defun eval-mark--disable ()
  "Disable function `eval-mark-mode'."
  (dolist (command eval-mark-commands-before)
    (advice-remove command #'eval-mark--deactivate-mark))
  (dolist (command eval-mark-commands-after)
    (advice-remove command #'eval-mark--deactivate-mark)))

;;;###autoload
(define-minor-mode eval-mark-mode
  "Minor mode `eval-mark-mode'."
  :global t
  :require 'eval-mark-mode
  :group 'eval-mark
  (if eval-mark-mode (eval-mark--enable) (eval-mark--disable)))

(provide 'eval-mark)
;;; eval-mark.el ends here
