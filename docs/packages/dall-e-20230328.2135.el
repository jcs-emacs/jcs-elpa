;;; dall-e.el --- Use DALL-E inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/dall-e
;; Package-Version: 20230328.2135
;; Package-Commit: fee552631f7b6d4bdcae8e928f7c3554aa39fa6f
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (openai "0.1.0") (spinner "1.7.4"))
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

(require 'cl-lib)
(require 'let-alist)
(require 'subr-x)

(require 'openai-image)
(require 'spinner)

(defgroup dall-e nil
  "Use DALL-E inside Emacs."
  :prefix "dall-e-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/dall-e"))

(defcustom dall-e-n 1
  "The number of images to generate.  Must be between 1 and 10."
  :type 'integer
  :group 'dall-e)

(defcustom dall-e-size "1024x1024"
  "The size of the generated images.

Must be one of `256x256', `512x512', or `1024x1024'."
  :type 'string
  :group 'dall-e)

(provide 'dall-e)
;;; dall-e.el ends here
