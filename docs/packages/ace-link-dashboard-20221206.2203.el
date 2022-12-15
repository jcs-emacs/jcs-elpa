;;; ace-link-dashboard.el --- Ace link for dashboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  emacs-dashboard maintainers

;; Author: Ricardo Arredondo
;; Maintainer: Ricardo Arredondo <ricardo.richo@gmail.com>
;;             Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-dashboard/ace-link-dashboard
;; Package-Version: 20221206.2203
;; Package-Commit: 459d1877269d8c0ac117e6a1915a40c716e88eab
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (avy "0.5.0"))
;; Keywords: tools

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
;; Ace link for emacs-dashboard
;;

;;; Code:

(require 'wid-edit)

(require 'avy)

;;;###autoload
(defun ace-link-dashboard ()
  "Open a visible link in a `dashboard-mode' buffer."
  (interactive)
  (let ((pt (avy-with 'ace-link-dashboard
              (avy-process
               (mapcar #'cdr (ace-link-dashboard--collect))
               (avy--style-fn avy-style)))))
    (ace-link-dashboard--action pt)))

;;;###autoload
(defun ace-link-dashboard-remove ()
  "Call remove action on widget."
  (interactive)
  (let ((point (avy-with 'ace-link-dashboard-remove
                 (avy-process
                  (mapcar #'cdr (ace-link-dashboard--collect))
                  (avy--style-fn avy-style)))))
    (ace-link-dashboard--remove point)))

(defun ace-link-dashboard--action (point)
  "Call action at POINT when widget is selected."
  (funcall 'widget-button-press point))

(defun ace-link-dashboard--remove (_point)
  "Call remove action on item at POINT."
  (dashboard-remove-item-under))

(defun ace-link-dashboard--collect ()
  "Collect all widgets in the current `dashboard-mode' buffer."
  (save-excursion
    (let ((previous-point (window-start))
          (candidates nil)
          (next-widget-point (lambda ()
                               (progn (widget-move 1)
                                      (point)))))
      (goto-char (window-start))
      (while (< previous-point (funcall next-widget-point))
        (setq previous-point (point))
        (push (cons (widget-at previous-point) previous-point) candidates))
      (nreverse candidates))))

(provide 'ace-link-dashboard)
;;; ace-link-dashboard.el ends here
