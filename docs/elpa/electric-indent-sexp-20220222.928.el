;;; electric-indent-sexp.el --- Automatically indent entire balanced expression block  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-02-22 17:10:55

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Automatically indent entire balanced expression block.
;; Keyword: indent sexp electric
;; Version: 0.1.0
;; Package-Version: 20220222.928
;; Package-Commit: d477ff0f29ea6046312ea98cf01b8608a13d0293
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/jcs-elpa/electric-indent-sexp

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
;; Automatically indent entire balanced expression block.
;;

;;; Code:

(defgroup electric-indent-sexp nil
  "Automatically indent entire balanced expression block."
  :prefix "electric-indent-sexp-"
  :group 'electricity
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/electric-indent-sexp"))

(defun electric-indent-sexp--post-self-insert (fn &rest args)
  "Execut around function `electric-indent-post-self-insert-function'."
  (when (apply fn args)
    (let ((inhibit-message t) message-log-max)
      (let ((end (point))
            (beg (save-excursion
                   (if (memq last-command-event electric-indent-chars)
                       (ignore-errors (backward-sexp))
                     (ignore-errors (forward-sexp)))
                   (point))))
        (unless (= beg end)
          (indent-region beg end))))))

(defun electric-indent-sexp--enable ()
  "Enable function `electric-indent-sexp-mode'."
  (advice-add 'electric-indent-post-self-insert-function :around #'electric-indent-sexp--post-self-insert))

(defun electric-indent-sexp--disable ()
  "Disable function `electric-indent-sexp-mode'."
  (advice-remove 'electric-indent-post-self-insert-function #'electric-indent-sexp--post-self-insert))

;;;###autoload
(define-minor-mode electric-indent-sexp-mode
  "Minor mode 'electric-indent-sexp-mode'."
  :global t
  :require 'electric-indent-sexp-mode
  :group 'electric-indent-sexp
  (if electric-indent-sexp-mode (electric-indent-sexp--enable) (electric-indent-sexp--disable)))

(provide 'electric-indent-sexp)
;;; electric-indent-sexp.el ends here
