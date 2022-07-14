;;; prt.el --- Progress Reporter Library  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-07-14 13:41:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/prt
;; Package-Version: 20220714.618
;; Package-Commit: e862eda951b35ee73e5ef646e94b2f4267e45a2c
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience

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
;; Progress Reporter Library
;;

;;; Code:

(defgroup prt nil
  "Progress Reporter Library."
  :prefix "prt-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/prt"))

(defmacro prt--silent (&rest body)
  "Execute BODY without write it to message buffer."
  (declare (indent 0) (debug t))
  `(let (message-log-max) ,@body))

;;;###autoload
(defun prt-create (message &optional min-value max-value current-value min-change min-time)
  "Create a progress reporter."
  (make-progress-reporter message min-value max-value current-value min-change min-time))

(defun prt-update (reporter &optional value suffix)
  "Report progress of an operation in the echo area."
  (progress-reporter-update reporter value suffix))

(defun prt-done (reporter &optional message)
  "Print reporter's message followed by word \"done\" in echo area."
  (prt--silent
    (progress-reporter-done reporter)
    (when message (message message))))

;;;###autoload
(defmacro prt-with (message &rest body)
  "Execute BODY with progress reporter in a scope."
  (declare (indent 1) (debug t))
  `(let ((rt (prt-create ,message))) ,@body))

(provide 'prt)
;;; prt.el ends here
