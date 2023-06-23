;;; font-lock-ext.el --- extensions to Emacs font-lock.el
;;
;; Copyright 2011-2012 Florian Kaufmann <sensorflo@gmail.com>
;; 
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://github.com/sensorflo/font-lock-ext/
;; Package-Version: 20171030.1445
;; Package-Commit: b6c82e8ac7996d96494a54454015a98ceb883feb
;; Created: 2011
;; Keywords: languages, faces
;; 
;; This file is not part of GNU Emacs.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;; Commentary: 
;; 
;; Currently it only adds some faces to the customize group font-lock-faces. The
;; plan is however that it will provide additional font lock keyword forms. As
;; font-lock.el states in it's commentary section, at least it's syntactic parse
;; abilities could be improved.
;;
;;; Code: 

(require 'font-lock)

(defface font-lock-unimportant
  '((((background dark)) (:foreground "gray80"))
    (t (:foreground "gray80")))
  "Face for hi-lock mode."
  :group 'font-lock-faces)

(defface font-lock-semi-unimportant
  '((((background dark)) (:foreground "gray80"))
    (t (:foreground "gray80")))
  "Face for hi-lock mode."
  :group 'font-lock-faces)

(defface font-lock-type-def
  '((((background dark)) (:foreground "green"))
    (t (:foreground "green")))
  "Face for hi-lock mode."
  :group 'font-lock-faces)

(defface font-lock-jump-keyword
  '((t nil))
  "Face for hi-lock mode."
  :group 'font-lock-faces)

;; font-lock.el says (see definition of the variable font-lock-comment-face)
;; that there is actually no need to create variables that specify face names.
;; However it seems to be needed all the same.
(defvar font-lock-semi-unimportant 'font-lock-semi-unimportant)
(defvar font-lock-unimportant 'font-lock-unimportant)
(defvar font-lock-jump-keyword 'font-lock-jump-keyword)


(provide 'font-lock-ext)
;;; font-lock-ext.el ends here
