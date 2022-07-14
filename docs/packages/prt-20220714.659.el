;;; prt.el --- Progress Reporter Library  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-07-14 13:41:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/prt
;; Package-Version: 20220714.659
;; Package-Commit: aca37951bc3596ec52c28df1fd472c8cbe11f280
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (spinner "1.7.4"))
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

(require 'spinner)

(defgroup prt nil
  "Progress Reporter Library."
  :prefix "prt-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/prt"))

(defcustom prt-pulse-characters (if (boundp 'progress-reporter--pulse-characters)
                                    progress-reporter--pulse-characters
                                  ["-" "\\" "|" "/"])
  "Characters to use for pulsing progress reporters."
  :type 'vector
  :group 'prt)

(defmacro prt--silent (&rest body)
  "Execute BODY without write it to message buffer."
  (declare (indent 0) (debug t))
  `(let (message-log-max) ,@body))

(defun prt--pulse-characters ()
  "Return a valid vector with pulse characters."
  (cond ((vectorp prt-pulse-characters) prt-pulse-characters)
        ((symbolp prt-pulse-characters) (cdr (assoc prt-pulse-characters spinner-types)))
        (t (user-error "Invalid pulse characters: %s" prt-pulse-characters))))

(defmacro prt--env (&rest body)
  "Execute BODY with setup environment."
  (declare (indent 0) (debug t))
  `(let ((progress-reporter--pulse-characters (prt--pulse-characters))) ,@body))

;;;###autoload
(defun prt-create (message &optional min-value max-value current-value min-change min-time)
  "Create a progress reporter."
  (prt--env (make-progress-reporter message min-value max-value current-value min-change min-time)))

(defun prt-update (reporter &optional value suffix)
  "Report progress of an operation in the echo area."
  (prt--env (progress-reporter-update reporter value suffix)))

(defun prt-done (reporter &optional message)
  "Print reporter's message followed by word \"done\" in echo area."
  (prt--env
    (prt--silent
      (progress-reporter-done reporter)
      (when message (message message)))))

;;;###autoload
(defmacro prt-with (message &rest body)
  "Execute BODY with progress reporter in a scope."
  (declare (indent 1) (debug t))
  `(let ((rt (prt-create ,message))) ,@body))

(provide 'prt)
;;; prt.el ends here
