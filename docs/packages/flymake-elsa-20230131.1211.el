;;; flymake-elsa.el --- Flymake integration for Elsa  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/flymake/flymake-elsa
;; Package-Version: 20230131.1211
;; Package-Commit: f4f05ab17c57486dcb715ee6f42db2d95a1737d8
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (flymake-easy "0.1"))
;; Keywords: lisp elsa

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
;; Flymake integration for Elsa
;;

;;; Code:

(require 'flymake)

(require 'flymake-easy)

(defgroup flymake-elsa nil
  "Flymake integration for Elsa."
  :prefix "flymake-elsa-"
  :group 'flymake
  :link '(url-link :tag "Github" "https://github.com/emacs-eask/flymake-elsa"))

(defconst flymake-elsa-err-line-patterns
  '(("^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):\\(error\\|warning\\):\\(.*\\)$" 1 2 3 5)))

(defun flymake-elsa-command (filename)
  "Construct a command that flymake can use to check elisp source."
  (list "eask" "exec" "elsa" filename))

;;;###autoload
(defun flymake-elsa-load ()
  "Configure flymake mode to check the current buffer's Elsa syntax.
This function is designed to be called in `emacs-lisp-mode-hook'; it does not
alter flymake's global configuration, so function `flymake-mode' alone will not
suffice."
  (interactive)
  (when (eq 'emacs-lisp-mode major-mode)
    (flymake-easy-load 'flymake-elsa-command
                       flymake-elsa-err-line-patterns
                       'tempdir
                       "elsa")))

(provide 'flymake-elsa)
;;; flymake-elsa.el ends here
