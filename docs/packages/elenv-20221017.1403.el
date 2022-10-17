;;; elenv.el --- Emacs Lisp environment  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/elenv
;; Package-Version: 20221017.1403
;; Package-Commit: ecdb206e396f527e66682b4e0654d42b4f1db650
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

(provide 'elenv)
;;; elenv.el ends here
