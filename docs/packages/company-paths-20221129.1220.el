;;; company-paths.el --- A company backend for paths  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/company-paths
;; Package-Version: 20221129.1220
;; Package-Commit: 69ad2a66708bd6d366cf7df910c083de376c09c9
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (company "0.8.12"))
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
;; A company backend for paths
;;

;;; Code:

(require 'company-files)

(defgroup company-paths nil
  "A company backend for paths."
  :prefix "company-paths-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/company-paths"))

(defcustom company-paths-complete-with-children nil
  "Non-nil to include children in the completion."
  :type 'boolean
  :group 'company-paths)

(defcustom company-paths-use-full-path nil
  "Non-nil to include children in the completion."
  :type 'boolean
  :group 'company-paths)

(defcustom company-paths-annotation-folder "📁"
  "Annotation string to indicate folder."
  :type 'string
  :group 'company-paths)

(defcustom company-paths-annotation-file "🗎"
  "Annotation string to indicate file."
  :type 'string
  :group 'company-paths)

;;
;; (@* "Core" )
;;

(defun company-paths--complete (prefix)
  "Pretty much the same with `company-files--complete'."
  (let* ((dir (file-name-directory prefix))
         (file (file-name-nondirectory prefix))
         (key (list file
                    (expand-file-name dir)
                    (nth 5 (file-attributes dir))))
         (completion-ignore-case read-file-name-completion-ignore-case))
    (unless (company-file--keys-match-p key (car company-files--completion-cache))
      (let* ((candidates (mapcar (if company-paths-use-full-path
                                     (lambda (f) (concat dir f))
                                   #'identity)
                                 (company-files--directory-files dir file)))
             (directories (unless (file-remote-p dir)
                            (cl-remove-if-not (lambda (f)
                                                (and (company-files--trailing-slash-p f)
                                                     (not (file-remote-p f))
                                                     (company-files--connected-p f)))
                                              candidates)))
             (children (and company-paths-complete-with-children
                            directories
                            (cl-mapcan (lambda (d)
                                         (mapcar (lambda (c) (concat d c))
                                                 (company-files--directory-files d "")))
                                       directories))))
        (setq company-files--completion-cache
              (cons key (append candidates children)))))
    (let ((candidates (all-completions file
                                       (cdr company-files--completion-cache))))
      (if company-paths-use-full-path candidates
        (mapcar (lambda (path) (string-replace dir "" path)) candidates)))))

(defun company-paths--annotation (arg)
  "Annotation for ARG."
  (if (string-suffix-p "/" arg) company-paths-annotation-folder
    company-paths-annotation-file))

(defun company-paths--post-completion (arg)
  "Post-completion for ARG."
  ;; Insert prefix
  (let ((inhibit-redisplay t))
    (forward-char (- 0 (length arg)))
    (insert (nth 1 (car company-files--completion-cache)))
    (forward-char (length arg)))
  ;; Respect variable `company-files-chop-trailing-slash'
  (funcall #'company-files--post-completion arg)
  (when (company-files--trailing-slash-p arg)
    ;; TODO: ..
    ))

;;;###autoload
(defun company-paths (command &optional arg &rest ignored)
  "Complete for paths.

Arguments COMMAND, ARG and IGNORED are standard arguments from `company-mode`."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-paths))
    (annotation (company-paths--annotation arg))
    (prefix (company-files--grab-existing-name))
    (candidates (company-paths--complete arg))
    (location (cons (dired-noselect
                     (file-name-directory (directory-file-name arg)))
                    1))
    (post-completion (company-paths--post-completion arg))
    (kind (if (string-suffix-p "/" arg) 'folder 'file))
    (sorted t)
    (no-cache t)))

(provide 'company-paths)
;;; company-paths.el ends here
