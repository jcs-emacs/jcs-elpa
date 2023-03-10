;;; license-templates.el --- Create LICENSE using GitHub API  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Shen, Jen-Chieh
;; Created date 2020-07-24 11:11:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/license-templates
;; Package-Version: 20230310.2031
;; Package-Commit: 1ddfaca61fbc6c5b553cfe84f3ccb5a34ab68840
;; Version: 0.1.3
;; Package-Requires: ((emacs "24.3") (request "0.3.0"))
;; Keywords: convenience license api template

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
;; Create LICENSE using GitHub API.
;;

;;; Code:

(require 'json)
(require 'url)

(require 'request)

(defgroup license-templates nil
  "Create LICENSE using GitHub API."
  :prefix "license-templates-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/license-templates"))

(defcustom license-templates-filename "LICENSE"
  "Filename when creating new license file."
  :type 'string
  :group 'license-templates)

(defvar license-templates-api-key ""
  "API key to get rid of the limit")

(defvar license-templates--keys nil
  "List of kesy of available templates.")

(defvar license-templates--info-list nil
  "List of license data information.")

(defvar license-templates--request-count 0
  "Count the completed request.")

(defvar license-templates--requested 0
  "The requested request.")

(defvar url-http-end-of-headers)

;;; Core

(defun license-templates--form-data (key name url content)
  "Form license data by KEY, NAME, URL, CONTENT."
  (list :key key :name name :url url :content content))

(defun license-templates--add-data-with-content (key name url)
  "Add data KEY, NAME, and URL, with content."
  (request url
    :type "GET"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer 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")))
    :parser 'json-read
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (let-alist data
         (cl-incf license-templates--request-count)
         (push (license-templates--form-data key name url .body) license-templates--info-list))))))

(defun license-templates--get-info ()
  "Get all necessary information."
  (setq license-templates--info-list nil
        license-templates--request-count 0
        license-templates--requested 0)
  (request "https://api.github.com/licenses"
    :type "GET"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer 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")))
    :parser 'json-read
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq license-templates--requested (length data))
       (mapc (lambda (json)
               (let-alist json
                 (license-templates--add-data-with-content .key .name .url)))
             data)))))

(defun license-templates--safe-get-info ()
  "Get the license information without refreshing cache."
  (cond ((not (= license-templates--requested license-templates--request-count))
         (user-error "Reuqest is not complete yet, please wait a while"))
        (t (unless license-templates--info-list
             (license-templates--get-info)))))

;;;###autoload
(defun license-templates-keys ()
  "Return list of keys of available license."
  (license-templates--safe-get-info)
  (unless license-templates--keys
    (dolist (data license-templates--info-list)
      (push (plist-get data :key) license-templates--keys))
    (setq license-templates--keys (sort license-templates--keys #'string-lessp)))
  license-templates--keys)

(defun license-templates--get-content-by-name (name)
  "Return license template by NAME."
  (license-templates--safe-get-info)
  (let ((content ""))
    (dolist (data license-templates--info-list)
      (when (equal (plist-get data :key) name)
        (setq content (plist-get data :content))))
    content))

;;;###autoload
(defun license-templates-insert (name)
  "Insert license for NAME."
  (interactive
   (list (completing-read "License template: "
                          (license-templates-keys)
                          nil t)))
  (insert (license-templates--get-content-by-name name)))

;;;###autoload
(defun license-templates-new-file (name &optional dir)
  "Create a license file with NAME in DIR."
  (interactive
   (list (completing-read "License template: "
                          (license-templates-keys)
                          nil t)
         (if current-prefix-arg
             (read-directory-name "Create license in directory: ")
           default-directory)))
  (let ((file (expand-file-name license-templates-filename dir)))
    (when (file-exists-p file)
      (user-error "Can't create '%s', because it already exists"
                  (abbreviate-file-name file)))
    (write-region (license-templates--get-content-by-name name) nil file)))

(provide 'license-templates)
;;; license-templates.el ends here
