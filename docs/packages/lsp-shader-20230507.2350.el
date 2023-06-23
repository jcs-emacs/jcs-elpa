;;; lsp-shader.el --- LSP Clients for ShaderLab  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/shader-ls/lsp-shader
;; Package-Version: 20230507.2350
;; Package-Commit: 77b14eba20a4be2243b05530a78bb14c3a1f2a9a
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (lsp-mode "6.1"))
;; Keywords: convenience shader

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
;; LSP Clients for ShaderLab.
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-shader nil
  "Settings for the ShaderLab Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/lsp-mode/lsp-shader"))

(defcustom lsp-shader-server-path nil
  "Path points for ShaderLab LSP.

This is only for development use."
  :type 'string
  :group 'lsp-shader)

(defcustom lsp-shader-active-modes
  '(shader-mode hlsl-mode glsl-mode)
  "List of major mode that work with shader-ls."
  :type 'list
  :group 'lsp-shader)

(defcustom lsp-shader-completion-word t
  "Non-nil to enable word completion."
  :type 'string
  :group 'lsp-shader)

(defun lsp-shader--cls-download-server (_client callback error-callback update?)
  "Install/update shader-ls language server using `dotnet tool'.

Will invoke CALLBACK or ERROR-CALLBACK based on result.
Will update if UPDATE? is t"
  (lsp-async-start-process
   callback
   error-callback
   "dotnet" "tool" (if update? "update" "install") "-g" "shader-ls"))

(defun lsp-shader--cls-test-shader-ls-present ()
  "Return non-nil if dotnet tool shader-ls is installed globally."
  (string-match-p "shader-ls"
                  (shell-command-to-string "dotnet tool list -g")))

(defun lsp-shader--server-command ()
  "Generate startup command for ShaderLab language server."
  (or (and lsp-shader-server-path
           (list lsp-shader-server-path "--stdio"))
      (list "shader-ls" "--stdio")))

(lsp-register-custom-settings
 `(("ShaderLab.CompletionWord" lsp-shader-completion-word)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-shader--server-command
                                        #'lsp-shader--cls-test-shader-ls-present)
  :priority -1
  :server-id 'shader-ls
  :major-modes lsp-shader-active-modes
  :add-on? t
  :download-server-fn #'lsp-shader--cls-download-server))

(provide 'lsp-shader)
;;; lsp-shader.el ends here
