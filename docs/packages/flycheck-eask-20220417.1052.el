;;; flycheck-eask.el --- Eask support in Flycheck  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-eask/flycheck-eask
;; Package-Version: 20220417.1052
;; Package-Commit: edd6cd43073667e9ca91820ff3e769ae81622ad1
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (flycheck "0.14"))
;; Keywords: eask

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

(flycheck-define-checker eask
  "A linter for textlint."
  :command ("eask" "check-eask")
  :error-patterns
  ((error line-start (file-name) ":" line ":" column " Error: "
          (message (one-or-more not-newline)
                   (zero-or-more "\n" (any " ") (one-or-more not-newline)))
          line-end)
   (warning line-start (file-name) ":" line ":" column " Warning: "
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (eask-mode))

;;;###autoload
(defun flycheck-eask-setup ()
  "Setup flycheck-package."
  (interactive)
  (add-to-list 'flycheck-checkers 'eask))

(provide 'flycheck-eask)
;;; flycheck-eask.el ends here
