;;; lsp-compose.el --- lsp-mode client for compose server -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.1
;; Package-Version: 20200515.1833
;; Package-Commit: 44df194778503cd2e29e907af4c6989c7ea27d1c
;; Package-Requires: ((emacs "26"))
;; URL: https://github.com/lepisma/compose-language-server.el

;;; Commentary:

;; lsp-mode client for compose server
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-compose nil
  "Language Server Protocol client for compose server."
  :group 'lsp-mode)

(defcustom lsp-compose-kenlm-model-path nil
  "Model file to be used by kenlm completor."
  :group 'lsp-compose)

(defcustom lsp-compose-kenlm-prefix-size 7
  "Number of tokens to use for prefix."
  :group 'lsp-compose)

(defun lsp-compose-command ()
  "Return command to run for the server."
  (list "compose-ls" "--stdio"
        "--kenlm-model-path" lsp-compose-kenlm-model-path
        "--kenlm-prefix-size" (number-to-string lsp-compose-kenlm-prefix-size)))

(add-to-list 'lsp-language-id-configuration '(mu4e-compose-mode . "mu4e-compose"))
(add-hook 'mu4e-compose-mode-hook (lambda () (setq-local company-minimum-prefix-length 0)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-compose-command)
  :major-modes '(mu4e-compose-mode)
  :server-id 'compose-ls))

(provide 'lsp-compose)

;;; lsp-compose.el ends here
