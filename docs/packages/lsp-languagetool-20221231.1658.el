;;; lsp-languagetool.el --- LSP Client for LanguageTool  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Shen, Jen-Chieh
;; Created date 2021-04-05 23:30:46

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-languagetool/lsp-languagetool
;; Package-Version: 20221231.1658
;; Package-Commit: cf688643a01a685fbcfef121f82119cc1ed57c79
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (lsp-mode "6.1"))
;; Keywords: convenience lsp languagetool checker

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
;; LSP server implementation for LanguageTool
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-languagetool nil
  "Settings for the LanguageTool Language Server.

https://github.com/languagetool-language-server/languagetool-languageserver"
  :prefix "lsp-languagetool-"
  :group 'lsp-mode
  :link '(url-link :tag "Github" "https://github.com/emacs-languagetool/lsp-languagetool"))

(provide 'lsp-languagetool)
;;; lsp-languagetool.el ends here
