;;; fof.el --- Default configuration for `ff-find-other-file'  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/fof
;; Package-Version: 20221008.650
;; Package-Commit: e1d5587209620dabfcf7af28ec3d86c71bf60862
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ffpc "0.1.0"))
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
;; Default configuration for `ff-find-other-file'.
;;

;;; Code:

(require 'find-file)

(require 'ffpc)

(defgroup fof nil
  "Default configuration for `ff-find-other-file'."
  :prefix "fof-"
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/fof"))

(defcustom fof-file-alist
  (append
   cc-other-file-alist
   ;; aspx
   `(("\\.aspx$"    (".aspx.cs"))
     ("\\.aspx.cs$" (".aspx")))
   ;; coffee
   `(("\\.coffee$" (".js"))
     ("\\.js$"     (".coffee"))))
  "Default for `ff-other-file-alist'."
  :type 'alist
  :group 'fof)

(defun fof--project-dirs ()
  "Return a list of project directories."
  (ffpc-directories-ignored-dir (or (ignore-errors (project-root (project-current)))
                                    default-directory)))

;;;###autoload
(defun fof ()
  "Find corresponding file to current buffer."
  (interactive)
  (let ((ff-other-file-alist fof-file-alist)
        (ff-search-directories (fof--project-dirs)))
    (call-interactively #'ff-find-other-file)))

;;;###autoload
(defun fof-other-window ()
  "Like `fof' but with other window."
  (interactive)
  (let ((ff-always-in-other-window t)) (fof)))

(provide 'fof)
;;; fof.el ends here
