;;; github-tags.el --- Retrieve tags information through GitHub API  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-08-08 16:14:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/github-tags
;; Package-Version: 20221215.1738
;; Package-Commit: 1c96ab259c334c10e8ef69b70b8e4e6df3322a66
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: vc github tags

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
;; Retrieve tags information through GitHub API
;;

;;; Code:

(require 'json)
(require 'url)

(defgroup github-tags nil
  "Retrieve tags information through GitHub API."
  :prefix "github-tags-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/github-tags"))

(defconst github-tags-api "https://api.github.com/repos/%s/tags"
  "API url to GitHub tags.")

;;
;; (@* "Externals" )
;;

(defvar url-http-end-of-headers)

(declare-function msgu-silent "ext:msgu-silent.el")

;;
;; (@* "Core" )
;;

(defun github-tags--url-to-json (url)
  "Get data by URL and convert it to JSON."
  (with-current-buffer (url-retrieve-synchronously url)
    (set-buffer-multibyte t)
    (goto-char url-http-end-of-headers)
    (prog1 (let ((json-array-type 'list)) (json-read))
      (kill-buffer))))

;;;###autoload
(defun github-tags (path)
  "Retrive tags data for PATH from GitHub API."
  (let* ((names) (zipball-urls) (tarball-urls) (commits) (node-ids)
         (url (format github-tags-api path))
         (data (ignore-errors
                 (if (featurep 'msgu)
                     (msgu-silent (github-tags--url-to-json url))
                   (github-tags--url-to-json url))))
         (msg-err (cdr (assoc 'message data))))
    (when msg-err (user-error "[ERROR] %s, %s" msg-err url))
    (dolist (tag data)
      (let ((name (cdr (assoc 'name tag)))
            (zipball (cdr (assoc 'zipball_url tag)))
            (tarball (cdr (assoc 'tarball_url tag)))
            (commit (cdr (assoc 'commit tag)))
            (nodeId (cdr (assoc 'node_id tag))))
        (push name names)
        (push zipball zipball-urls)
        (push tarball tarball-urls)
        (push commit commits)
        (push nodeId node-ids)))
    (setq names (reverse names)
          zipball-urls (reverse zipball-urls)
          tarball-urls (reverse tarball-urls)
          commits (reverse commits)
          node-ids (reverse node-ids))
    (cons data `( :names ,names :zipball-urls ,zipball-urls
                  :tarball-urls ,tarball-urls
                  :commits ,commits
                  :node-ids ,node-ids))))

(provide 'github-tags)
;;; github-tags.el ends here
