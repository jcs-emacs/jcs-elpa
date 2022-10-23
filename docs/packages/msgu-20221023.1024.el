;;; msgu.el --- Utility functions help output the messages  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/msgu
;; Package-Version: 20221023.1024
;; Package-Commit: 0d95ab830391c697c6b0eb117d557b83ce4827fa
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
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

(defcustom msgu-log-max (or (ignore-errors (default-value 'message-log-max))
                            1000)
  "`message-log-max' for unsilencing."
  :type 'integer
  :group 'msgu)

;;;###autoload
(defmacro msgu-inhibit-log (&rest body)
  "Execute BODY without write it to message buffer."
  (declare (indent 0) (debug t))
  `(let (message-log-max) ,@body))

;;;###autoload
(defmacro msgu-silent (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(msgu-inhibit-log
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) ,@body))))

;;;###autoload
(defmacro msgu-unsilent (&rest body)
  "Execute BODY with ensuring message log."
  (declare (indent 0) (debug t))
  `(let ((message-log-max msgu-log-max)) ,@body))

;;
;; (@* "Util" )
;;

(defcustom msgu-sleep-seconds 0.4
  "Default seconds to sleep after calling `msgu-sleep' function."
  :type 'number
  :group 'msgu)

(defcustom msgu-sit-seconds 100
  "Default seconds to sit after calling `msgu-sit' function."
  :type 'number
  :group 'msgu)

;;;###autoload
(defun msgu-sleep (&optional seconds milliseconds)
  "Wrap `sleep-for' function width default SECONDS and MILLISECONDS."
  (sleep-for (or seconds msgu-sleep-seconds) milliseconds))

;;;###autoload
(defun msgu-sit (&optional seconds nodisp)
  "Wrap `sit-for' function with default SECONDS and NODISP."
  (sit-for (or seconds msgu-sit-seconds) nodisp))

;;
;; (@* "Color" )
;;

;; TOPIC: How to preserve color in *Messages* buffer?
;; SOURCE: https://emacs.stackexchange.com/questions/20171/how-to-preserve-color-in-messages-buffer

(defun msgu-color (fmt &rest args)
  "Like function `message' but preserve color in the buffer.

Arguments FMT and ARGS are used for format message."
  (msgu-inhibit-log (apply 'message fmt args))
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (apply 'format fmt args))))))

;;
;; (@* "Others" )
;;

(defcustom msgu-currnet-format "%s\n\n"
  "String use to format current message."
  :type 'string
  :group 'msgu)

;;;###autoload
(defun msgu-current (fmt &rest args)
  "Log messages with current message on top if available.

Arguments FMT and ARGS are used for format message."
  (message "%s%s"
           (if (current-message) (format msgu-currnet-format (current-message)) "")
           (apply #'format fmt args)))

(provide 'msgu)
;;; msgu.el ends here
