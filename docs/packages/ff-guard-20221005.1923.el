;;; ff-guard.el ---  Create parent directory for non-existent file  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/ff-guard
;; Package-Version: 20221005.1923
;; Package-Commit: 4c6079c1d3fe80d8f116654c646648bc420a9bad
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (f "0.20.0") (s "1.9.0"))
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
;;  Create parent directory for non-existent file.
;;

;;; Code:

(require 'f)
(require 's)

(defgroup ff-guard nil
  "Create parent directory for non-existent file."
  :prefix "ff-guard-"
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/ff-guard"))

(defvar ff-guard--current-created-parent-dir-path nil
  "Globally record the virutally created parent dir path.")

(defvar-local ff-guard--created-parent-dir-path nil
  "Record down the created parent directory path.")

(defconst ff-guard--msg-create-parent-directory "%s

The file has no parent directory, and you are trying to create the file there.
Do you want to create it? "
  "Message when creating parent directory.")

;;
;; (@* "Entry" )
;;

(defun ff-guard--enable ()
  "Enable `ff-guard'."
  (add-to-list 'find-file-not-found-functions #'ff-guard--create-non-existent-directory)
  (advice-add 'find-file :after #'ff-guard--find-file)
  (advice-add 'save-buffer :after #'ff-guard--save-buffer)
  (advice-add 'kill-this-buffer :before #'ff-guard--kill-this-buffer))

(defun ff-guard--disable ()
  "Disable `ff-guard'."
  (setq find-file-not-found-functions
        (remove #'ff-guard--create-non-existent-directory find-file-not-found-functions))
  (advice-remove 'find-file #'ff-guard--find-file)
  (advice-remove 'save-buffer #'ff-guard--save-buffer)
  (advice-remove 'kill-this-buffer #'ff-guard--kill-this-buffer))

;;;###autoload
(define-minor-mode ff-guard-mode
  "Minor mode `ff-guard-mode'."
  :global t
  :require 'ff-guard
  :group 'ff-guard
  (if ff-guard-mode (ff-guard--enable) (ff-guard--disable)))

;;
;; (@* "Util" )
;;

(defun ff-guard--directory-p (path)
  "Return non-nil if PATH is a directory path."
  (and (file-exists-p path) (file-directory-p path)))

;;
;; (@* "Core" )
;;

(defun ff-guard--find-starting-not-exists-dir-path (path &optional d-f)
  "Return the not exists directory path by PATH; D-F is optional default directory."
  (let* ((d-f (or d-f default-directory))
         (virtual-path (s-replace d-f "" path))
         (split-paths (f-split virtual-path)) (split-path-item "")
         (prev-path (f-slash d-f)) (test-path prev-path)
         (index 0) break-it result-path)
    (while (and (< index (length split-paths)) (not break-it))
      (setq split-path-item (nth index split-paths)
            test-path (f-slash (f-join test-path split-path-item)))
      (unless (file-directory-p test-path)
        (setq result-path prev-path break-it t))
      (setq prev-path test-path
            index (1+ index)))
    (unless result-path (setq result-path prev-path))
    (f-slash result-path)))

(defun ff-guard--create-non-existent-directory ()
  "Create the parent directory if not exists."
  (let* ((parent-directory (file-name-directory buffer-file-name))
         (non-virtual-path (ff-guard--find-starting-not-exists-dir-path parent-directory))
         (created-path (s-replace non-virtual-path "" parent-directory)))
    (when (and (not (ff-guard--directory-p parent-directory))
               (y-or-n-p (format ff-guard--msg-create-parent-directory parent-directory)))
      (make-directory parent-directory t)
      (setq ff-guard--current-created-parent-dir-path created-path))))

(defun ff-guard--find-file (&rest _)
  "For `find-file-hook'."
  (when ff-guard--current-created-parent-dir-path
    (setq ff-guard--created-parent-dir-path ff-guard--current-created-parent-dir-path
          ff-guard--current-created-parent-dir-path nil)))

(defun ff-guard--save-buffer (&rest _)
  "For `save-buffer' advice."
  (setq ff-guard--created-parent-dir-path nil))

(defun ff-guard--kill-this-buffer (&rest _)
  "For `kill-this-buffer' advice."
  (when (and ff-guard-mode
             ff-guard--created-parent-dir-path)  ; Remove virtual parent directory.
    (let* ((topest-dir (nth 0 (f-split ff-guard--created-parent-dir-path)))
           (create-dir (s-replace ff-guard--created-parent-dir-path "" default-directory))
           (del-path (f-slash (concat create-dir topest-dir))))
      (ignore-errors (delete-directory del-path t))
      (message "[INFO] Remove parent directory that were virtual => '%s'" del-path))))

(provide 'ff-guard)
;;; ff-guard.el ends here
