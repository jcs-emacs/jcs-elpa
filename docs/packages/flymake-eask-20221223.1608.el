;;; flymake-eask.el --- Eask support in Flymake  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/flymake/flymake-eask
;; Package-Version: 20221223.1608
;; Package-Commit: a6c87f4aab9530cc24a463c1a8dd63770e9482bd
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (flymake-easy "0.1"))
;; Keywords: lisp eask

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
;; Eask support in Flymake.
;;

;;; Code:

(require 'flymake)

(require 'flymake-easy)

(defgroup flymake-eask nil
  "Eask support for Flymake."
  :prefix "flymake-eask-"
  :group 'flymake
  :link '(url-link :tag "Github" "https://github.com/emacs-eask/flymake-eask"))

(defconst flymake-eask-err-line-patterns
  '(("^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(Error\\|Warning\\): \\(.*\\)$" 1 2 3 5)))

(defun flymake-eask-command (filename)
  "Construct a command that flymake can use to check Eask source."
  (list "eask" "check-eask" filename))

;;;###autoload
(defun flymake-eask-load ()
  "Configure flymake mode to check the current buffer's Eask syntax.

This function is designed to be called in `eask-mode-hook'; it does not alter
flymake's global configuration, so function `flymake-mode' alone will not
suffice."
  (interactive)
  (when (eq 'eask-mode major-mode)
    (flymake-easy-load 'flymake-eask-command
                       flymake-eask-err-line-patterns
                       'tempdir
                       "eask")))

(provide 'flymake-eask)
;;; flymake-eask.el ends here
