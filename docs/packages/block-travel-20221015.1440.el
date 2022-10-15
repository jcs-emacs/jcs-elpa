;;; block-travel.el --- Move to previous/next blank line  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs090218/block-travel
;; Package-Version: 20221015.1440
;; Package-Commit: 742fc147ffa93fa6174f2337d3892fd81ca8e868
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
;; Move to previous/next blank line
;;

;;; Code:

;;;###autoload
(defun block-travel-up (&optional _)
  "Move to the previous line containing nothing but whitespaces or tabs."
  (interactive "^P")
  (let ((sr-pt (save-excursion (re-search-backward "^[ \t]*\n" nil t))))
    (goto-char (or sr-pt (point-min)))))

;;;###autoload
(defun block-travel-down (&optional _)
  "Move to the next line containing nothing but whitespaces or tabs."
  (interactive "^P")
  (when (jcs-current-line-empty-p) (forward-line 1))
  (let ((sr-pt (save-excursion (re-search-forward "^[ \t]*\n" nil t))))
    (goto-char (or sr-pt (point-max)))
    (when sr-pt (forward-line -1))))

(provide 'block-travel)
;;; block-travel.el ends here
