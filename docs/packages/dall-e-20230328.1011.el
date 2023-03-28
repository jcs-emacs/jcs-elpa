;;; dall-e.el --- Use DALL-E inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs090218/dall-e
;; Package-Version: 20230328.1011
;; Package-Commit: 3429d6ed435d1171fccff94cfd3133dd09147ca9
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (openai "0.1.0"))
;; Keywords: comm dall-e

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
;; Use DALL-E inside Emacs.
;;

;;; Code:

(require 'openai-image)

(defgroup dall-e nil
  "Use DALL-E inside Emacs."
  :prefix "dall-e-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/dall-e"))

(provide 'dall-e)
;;; dall-e.el ends here
