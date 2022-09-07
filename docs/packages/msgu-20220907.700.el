;;; msgu.el --- Utility functions help output the messages  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/msgu
;; Package-Version: 20220907.700
;; Package-Commit: 33a56c7744f38abc6f9bc1b87e0970bbaf000a4a
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
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
;; Utility functions help output the messages.
;;

;;; Code:

(defgroup msgu nil
  "Utility functions help output the messages."
  :prefix "msgu-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/msgu"))

(defmacro msgu-unsilent (&rest body)
  "Execute BODY with ensuring message log."
  (declare (indent 0) (debug t))
  `(let ((message-log-max 1000)) ,@body))

;;;###autoload
(defmacro msgu-silent (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let (message-log-max)
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) ,@body))))

(defmacro msgu-no-log-apply (&rest body)
  "Execute BODY without write it to message buffer."
  (declare (indent 0) (debug t))
  `(let (message-log-max) ,@body))

;;;###autoload
(defun msgu-current (fmt &rest args)
  "Log messages with current message on top if available."
  (message "%s%s"
           (if (current-message) (concat (current-message) "\n\n") "")
           (apply #'format fmt args)))

(provide 'msgu)
;;; msgu.el ends here
