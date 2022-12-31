;;; flycheck-eask.el --- Eask support in Flycheck  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-eask/flycheck-eask
;; Package-Version: 20221231.1640
;; Package-Commit: a73bf0b3f610cce4d664b486e9e189820a14e18e
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (flycheck "0.14"))
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
;; Eask support in Flycheck
;;

;;; Code:

(require 'flycheck)

(defgroup flycheck-eask nil
  "Eask support for Flycheck."
  :prefix "flycheck-eask-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/emacs-eask/flycheck-eask"))

(flycheck-def-args-var flycheck-eask-args (eask)
  :package-version '(flycheck-eask . "0.1.0"))

(defun flycheck-eask-parse-lint (output checker buffer)
  "Parse Eask-file lint errors from JSON OUTPUT.

CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively."
  (let (errors)
    (seq-map (lambda (message)
               (dolist (data (cdr message))
                 (let-alist data
                   (push (flycheck-error-new-at
                          .range.start.line
                          (1+ .range.start.col)
                          (pcase (car message)
                            (`warnings 'warning)
                            (`errors 'error)
                            (_ 'warning))
                          .message
                          :id .code
                          :checker checker
                          :buffer buffer
                          :filename .filename
                          :end-line .range.end.line
                          :end-column (1+ .range.end.col))
                         errors))))
             (car (flycheck-parse-json output)))
    (nreverse errors)))

(flycheck-define-checker eask
  "A linter for Eask-file."
  :command ("eask" "check-eask" "--json"
            (eval flycheck-eask-args)
            source)
  :error-parser flycheck-eask-parse-lint
  :modes (eask-mode))

;;;###autoload
(defun flycheck-eask-setup ()
  "Setup flycheck-package."
  (interactive)
  (add-to-list 'flycheck-checkers 'eask))

(provide 'flycheck-eask)
;;; flycheck-eask.el ends here
