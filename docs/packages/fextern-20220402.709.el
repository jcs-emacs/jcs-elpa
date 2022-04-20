;;; fextern.el --- Record file external stats  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-03-08 17:30:07

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Record file external stats.
;; Keyword: externally file stats
;; Version: 0.1.0
;; Package-Version: 20220402.709
;; Package-Commit: 23f44d898e622ee90d01f09c480c671b0ba8130d
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/emacs-vs/fextern

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
;; Record file external stats.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar-local fextern-buffer-save-string-md5 nil
  "Buffer string when buffer is saved; this value encrypted with md5 algorithm.
This variable is used to check if file are edited externally.")

(defvar-local fextern-buffer-newly-created nil
  "Set to t if buffer is newly created.")

;;;###autoload
(defun fextern-update-buffer-save-string (&rest _)
  "Update variable `fextern-buffer-save-string-md5' once."
  (setq fextern-buffer-save-string-md5 (md5 (buffer-string))))

;;;###autoload
(defun fextern-after-save-buffer (&rest _)
  "Advice after `save-buffer'."
  (fextern-update-buffer-save-string)
  (setq fextern-buffer-newly-created nil))

;;;###autoload
(defun fextern-find-file (&rest _)
  "Hook `find-file'."
  (fextern-update-buffer-save-string)
  (unless (file-exists-p buffer-file-name)
    (setq fextern-buffer-newly-created t)))

;;;###autoload
(advice-add 'save-buffer :after #'fextern-after-save-buffer)
;;;###autoload
(add-hook 'find-file-hook #'fextern-find-file)

;;
;; (@* "Util" )
;;

(defun fextern--file-content (path)
  "Return PATH file content."
  (if (file-exists-p path)
      (with-temp-buffer (insert-file-contents path) (buffer-string))
    ""))

(defun fextern--valid-buffer-p (buffer)
  "Return non-nil if BUFFER does exist on disk."
  (when-let ((bfn (buffer-file-name buffer))) (file-exists-p bfn)))

(defun fextern--valid-buffer-list ()
  "Return a list of valid buffers."
  (cl-remove-if-not #'fextern--valid-buffer-p (buffer-list)))

;;
;; (@* "Core" )
;;

;;;###autoload
(defun fextern-buffer-edit-externally-p (&optional buf)
  "Return non-nil if BUF is edited externally."
  (let* ((buf (or buf (current-buffer)))
         (path (buffer-file-name buf))
         (buffer-saved-md5 (with-current-buffer buf fextern-buffer-save-string-md5))
         (file-content (fextern--file-content path))
         (file-content-md5 (md5 file-content)))
    (not (equal file-content-md5 buffer-saved-md5))))

;;;###autoload
(defun fextern-buffers-edit-externally ()
  "Return a list of buffers edit externally."
  (cl-remove-if-not
   (lambda (buf) (fextern-buffer-edit-externally-p (get-buffer buf)))
   (fextern--valid-buffer-list)))

(provide 'fextern)
;;; fextern.el ends here
