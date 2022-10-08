;;; fof.el --- Default configuration for `ff-find-other-file'  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/fof
;; Package-Version: 20221008.622
;; Package-Commit: 0a7257d913d648f50f3ac2b7735141be74095e19
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
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


(defgroup fof nil
  "Default configuration for `ff-find-other-file'."
  :prefix "fof-"
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/fof"))

(defcustom fof-file-alist
  '(;; C
    ("\\.h$"   (".c" ".cpp" ".cin" ".m"))
    ("\\.c$"   (".h" ".hpp" ".hin"))
    ;; C++
    ("\\.hpp$" (".cpp" ".c" ".cin" ".m"))
    ("\\.cpp$" (".hpp" ".h" ".hin"))
    ("\\.hin$" (".cin" ".c" ".cpp" ".m"))
    ("\\.cin$" (".hin" ".h" ".hpp"))
    ;; Objective-C
    ("\\.m$"   (".h" ".hpp" ".hin"))
    ;; aspx
    ("\\.aspx$"    (".aspx.cs"))
    ("\\.aspx.cs$" (".aspx"))
    ;; coffee
    ("\\.coffee$" (".js"))
    ("\\.js$"     (".coffee")))
  "Default for `ff-other-file-alist'."
  :type 'alist
  :group 'fof)

;;;###autoload
(defun fof ()
  "Find corresponding file to current buffer."
  (interactive)
  (let ((ff-other-file-alist fof-file-alist))
    (call-interactively #'ff-find-other-file)))

;;;###autoload
(defun fof-other-window ()
  "Like `fof' but with other window."
  (interactive)
  (let ((ff-always-in-other-window t)) (fof)))

(provide 'fof)
;;; fof.el ends here
