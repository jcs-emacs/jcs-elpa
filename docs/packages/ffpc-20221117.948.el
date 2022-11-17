;;; ffpc.el --- Find file in project or current directory  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/ffpc
;; Package-Version: 20221117.948
;; Package-Commit: 3f1a33fc0150e9459594006d2b234c8ace3ae675
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (dash "2.12.0") (f "0.20.0"))
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
;; Find file in project or current directory.
;;

;;; Code:

(require 'project)

(require 'dash)
(require 'f)

(defgroup ffpc nil
  "Find file in project or current directory."
  :prefix "ffpc-"
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/ffpc"))

;;
;; (@* "Util" )
;;

(defun ffpc-directories-ignored-dir (path &optional rec)
  "Find all directories in PATH by ignored common directories with FN and REC."
  (let ((dirs (f-directories path)) valid-dirs final-dirs)
    (dolist (dir dirs)
      (unless (member (f-filename (f-slash dir)) project-vc-ignores)
        (push dir valid-dirs)))
    (when rec
      (dolist (dir valid-dirs)
        (push (ffpc-directories-ignored-dir dir rec) final-dirs)))
    (setq valid-dirs (reverse valid-dirs)
          final-dirs (reverse final-dirs))
    (-flatten (append valid-dirs final-dirs))))

(defun ffpc-files-ignored-dir (path &optional fn rec)
  "Find all files in PATH by ignored common directories with FN and REC."
  (let ((dirs (append (list path) (ffpc-directories-ignored-dir path rec)))
        files)
    (dolist (dir dirs)
      (when-let ((fs (ignore-errors (f-files dir fn))))
        (push fs  files)))
    (-flatten (reverse files))))

;;
;; (@* "API" )
;;

;;;###autoload
(defun ffpc-select-file-current-dir (filename title)
  "Find FILENAME in current directory.

Argument FILENAME accept regular expression string.

Argument TITLE is a string used when there are more than one matches."
  (let* ((target-files
          (ffpc-files-ignored-dir default-directory
                                  (lambda (file)
                                    (string-match-p filename (f-filename file)))))
         (target-files-len (length target-files)))
    (when (zerop target-files-len)
      (user-error "[ERROR] No file '%s' found in the current directory" filename))
    (if (= target-files-len 1)
        (nth 0 target-files)  ; If only one file found, just get that file.
      (completing-read title target-files))))  ; Get the selected file.

;;;###autoload
(defun ffpc-select-file-in-project (filename title)
  "Find FILENAME in current project.

Argument FILENAME accept regular expression string.

Argument TITLE is a string used when there are more than one matches."
  (let ((project-dir (jcs-project-root)) target-files target-files-len)
    ;; Do the find file only when the project directory exists.
    (when project-dir
      (setq target-files
            (ffpc-files-ignored-dir project-dir
                                    (lambda (file)
                                      (string-match-p filename (f-filename file)))
                                    t)))
    (when target-files (setq target-files-len (length target-files)))
    (unless target-files-len
      (user-error "[ERROR] No file '%s' found in project, make sure the project root exists" filename))
    (if (= target-files-len 1)
        (nth 0 target-files)  ; If only one file found, just get that file.
      (completing-read title target-files))))  ; Get the selected file.

;;;###autoload
(defun ffpc-project-or-current-dir (filename title)
  "Find the file from project root, if not found find it in current directory.

Return full path if found, else error prompt.  FILENAME to search in project
or current directory.  TITLE search uses regexp, meaning it could found
multiple files at a time.  We need a title to present which file to select."
  (if-let ((filepath
            (or (ignore-errors (ffpc-select-file-current-dir filename title))
                (ignore-errors (ffpc-select-file-in-project filename title)))))
      filepath
    (user-error
     "No valid file found in either current directory or project, %s" filename)))

(provide 'ffpc)
;;; ffpc.el ends here
