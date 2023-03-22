;;; elenv.el --- Emacs Lisp environment  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/elenv
;; Package-Version: 20230322.543
;; Package-Commit: 27173a2e2f6b8a7c3fdc817b2fcbe625174e3ac9
;; Version: 0.1.0
;; Package-Requires: ((emacs "26."))
;; Keywords: maint

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
;; Emacs Lisp environment
;;

;;; Code:

;;
;; (@* "Operating System" )
;;

;;;###autoload
(defconst elenv-windows (memq system-type '(cygwin windows-nt ms-dos))
  "Microsoft Windows")

;;;###autoload
(defconst elenv-darwin (eq system-type 'darwin)
  "GNU-Darwin, macOS")

;;;###autoload
(defconst elenv-macos elenv-darwin
  "macOS")

;;;###autoload
(defconst elenv-linux (eq system-type 'gnu/linux)
  "Linux")

;;;###autoload
(defconst elenv-bsd (eq system-type 'gnu/kfreebsd)
  "BSD")

;;;###autoload
(defconst elenv-unix (memq system-type '(aix berkeley-unix hpux usg-unix-v))
  "Unix")

;;;###autoload
(defconst elenv-system-type
  (cond (elenv-windows 'dos)
        (elenv-macos   'mac)
        (elenv-linux   'unix)
        (elenv-bsd     'bsd)
        (t             'unknown))
  "Generic system type.")

;;;###autoload
(defmacro elenv-with-os (os &rest body)
  "Evaluate BODY by OS."
  (declare (indent 1))
  `(when (or (eq system-type ,os) (memq system-type ,os)) ,@body))

;;;###autoload
(defmacro elenv-with-windows (&rest body)
  "Evaluate BODY in Windows."
  (declare (indent 0)) `(when elenv-windows ,@body))

;;;###autoload
(defmacro elenv-with-macos (&rest body)
  "Evaluate BODY in macOS."
  (declare (indent 0)) `(when elenv-macos ,@body))

;;;###autoload
(defmacro elenv-with-linux (&rest body)
  "Evaluate BODY in Linux."
  (declare (indent 0)) `(when elenv-linux ,@body))

;;;###autoload
(defmacro elenv-with-bsd (&rest body)
  "Evaluate BODY in BSD."
  (declare (indent 0)) `(when elenv-bsd ,@body))

;;;###autoload
(defmacro elenv-with-unix (&rest body)
  "Evaluate BODY in Unix."
  (declare (indent 0)) `(when elenv-unix ,@body))

;;
;; (@* "Graphic" )
;;

;;;###autoload
(defconst elenv-graphic-p (display-graphic-p)
  "Return t if graphic mode.")

;;
;; (@* "Environment" )
;;

;;;###autoload
(defmacro elenv-if-env (variable then &rest else)
  "Evaluate THEN if VARIABLE is valid, we execute ELSE if not valid."
  (declare (indent 1))
  `(if-let ((value (getenv ,variable))) ,then ,@else))

;;;###autoload
(defmacro elenv-when-env (variable &rest body)
  "Evaluate BODY when VARIABLE is valid."
  (declare (indent 1))
  `(when-let ((value (getenv ,variable))) ,@body))

;;;###autoload
(defmacro elenv-unless-env (variable &rest body)
  "Evaluate BODY when VARIABLE is valid."
  (declare (indent 1))
  `(unless (getenv ,variable) ,@body))

;;
;; (@* "Executable" )
;;

(defmacro elenv--exec-find (command remote)
  "Find executable COMMAND.

For argument REMOTE, see function `executable-find' description."
  (let ((var (intern (format "elenv-exec-%s" command))))
    `(if (boundp ',var)
         (symbol-value ',var)
       (defvar ,var (executable-find ,command ,remote)
         (format "Variable generate it with `elenv' finding executable `%s'."
                 ,command))
       (symbol-value ',var))))

;;;###autoload
(defmacro elenv-if-exec (command remote then &rest else)
  "Evaluate BODY if COMMAND is found.

For argument REMOTE, see function `executable-find' description."
  (declare (indent 3))
  `(if-let ((value (elenv--exec-find ,command ,remote)))
       ,then
     ,@else))

;;;###autoload
(defmacro elenv-when-exec (command remote &rest body)
  "Evaluate BODY when COMMAND is found.

For argument REMOTE, see function `executable-find' description."
  (declare (indent 2))
  `(when-let ((value (elenv--exec-find ,command ,remote)))
     ,@body))

;;;###autoload
(defmacro elenv-unless-exec (command remote &rest body)
  "Evaluate BODY unless COMMAND is found.

For argument REMOTE, see function `executable-find' description."
  (declare (indent 2))
  `(unless (elenv--exec-find ,command ,remote)
     ,@body))

(provide 'elenv)
;;; elenv.el ends here
