;;; message-clean-mode.el --- Keep messages buffer clean  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-02-17 16:16:50

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Keep messages buffer clean
;; Keyword: messages clean
;; Version: 0.1.0
;; Package-Version: 20220217.940
;; Package-Commit: ec8ff6815adf65cc61367c618455da6b5753f40f
;; Package-Requires: ((emacs "24.4") (noflet "0.0.15"))
;; URL: https://github.com/jcs-elpa/message-clean-mode

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
;; Keep messages buffer clean.
;;

;;; Code:

(require 'noflet)

(defgroup message-clean nil
  "Keep messages buffer clean."
  :prefix "message-clean-mode-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/message-clean-mode"))

(defcustom message-clean-mode-commands
  '()
  "List of commands to inhibit log to *Messages* buffer."
  :type 'list
  :group 'message-clean)

(defcustom message-clean-mode-inhibit-echo nil
  "Non-nil to hide message from echo area."
  :type 'boolean
  :group 'message-clean)

(defcustom message-clean-mode-extra-handlers
  '(user-error)
  "List of function to mute."
  :type 'list
  :group 'message-clean)

(defmacro message-clean-mode--noflet (&rest body)
  "Defined local functions with `noflet'."
  (declare (indent 0) (debug t))
  `(dolist (fnc message-clean-mode-extra-handlers)
     (eval
      `(noflet ((,fnc (fmt &rest args)
                      (message "%s: %s" (format "%s" ',fnc)
                               (format-message fmt args))))
         ,@body))))

(defun message-clean-mode--mute (fnc &rest args)
  "Mute any commands (FNC, ARGS)."
  (let ((inhibit-message message-clean-mode-inhibit-echo)
        message-log-max)
    (message-clean-mode--noflet (apply fnc args))))

(defun message-clean-mode--enable ()
  "Enable `message-clean-mode'."
  (dolist (cmd message-clean-mode-commands)
    (advice-add cmd :around #'message-clean-mode--mute)))

(defun message-clean-mode--disable ()
  "Disable `message-clean-mode'."
  (dolist (cmd message-clean-mode-commands)
    (advice-remove cmd #'message-clean-mode--mute)))

;;;###autoload
(define-minor-mode message-clean-mode
  "Minor mode 'message-clean-mode'."
  :global t
  :require 'message-clean-mode
  :group 'message-clean
  (if message-clean-mode (message-clean-mode--enable) (message-clean-mode--disable)))

(provide 'message-clean-mode)
;;; message-clean-mode.el ends here
