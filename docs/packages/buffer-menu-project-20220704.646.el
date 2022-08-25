;;; buffer-menu-project.el --- List buffers relative to project  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-02-17 15:26:10

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/buffer-menu-project
;; Package-Version: 20220704.646
;; Package-Commit: 7a9505868f46d0658d5bc63a7b91bd4455434268
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (project "0.8.1") (f "0.20.0"))
;; Keywords: convenience buffer menu project

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
;; List buffers relative to project.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'project)
(require 'f)

(defgroup buffer-menu-project nil
  "List buffers relative to project."
  :prefix "buffer-menu-project-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/buffer-menu-project"))

(defun buffer-menu-project--current ()
  "Return project directory path."
  (cdr (project-current)))

(defun buffer-menu-project--project-name ()
  "Return project directory path."
  (f-base (cdr (project-current))))

(defvar-local buffer-menu-project-name nil "Name of the project name.")
(defvar-local buffer-menu-project-buffers nil "List of display project buffers.")

(defun buffer-menu-project--buffer-list ()
  "Return a list of buffers within the current project."
  (cl-remove-if-not
   (lambda (elm) (buffer-file-name elm))
   (project-buffers (project-current t))))

(defun buffer-menu-project--name ()
  "Return project name."
  (f-base (project-root (project-current t))))

(defun buffer-menu-project--list-buffers-noselect (name &optional files-only buffer-list)
  "Borrow from function `list-buffers-noselect'."
  (let ((old-buffer (current-buffer))
        (buffer (get-buffer-create (format "*Buffer List*: %s" name))))
    (with-current-buffer buffer
      (Buffer-menu-mode)
      (setq Buffer-menu-files-only
            (and files-only (>= (prefix-numeric-value files-only) 0)))
      (list-buffers--refresh buffer-list old-buffer)
      (tabulated-list-print))
    buffer))

(defun buffer-menu-project--buffers ()
  "Return buffer menu buffer for current project buffer."
  (when-let ((buffers (buffer-menu-project--buffer-list))
             (name (buffer-menu-project--name)))
    (with-current-buffer
        (buffer-menu-project--list-buffers-noselect name nil buffers)
      (setq buffer-menu-project-name name
            buffer-menu-project-buffers buffers)
      (current-buffer))))

;;;###autoload
(defun buffer-menu-project ()
  "Same with command `buffer-menu' but show only project buffers."
  (interactive)
  (when-let ((create (buffer-menu-project--buffers)))
    (switch-to-buffer create)))

;;;###autoload
(defun buffer-menu-project-other-window ()
  "Same with command `buffer-menu-project' but in other window."
  (interactive)
  (when-let ((create (buffer-menu-project--buffers)))
    (switch-to-buffer-other-window create)))

(provide 'buffer-menu-project)
;;; buffer-menu-project.el ends here
