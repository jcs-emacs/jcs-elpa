;;; jcs-poptip.el --- Generic popup tip  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-emacs/jcs-poptip
;; Package-Version: 20230415.512
;; Package-Commit: 4352098ace9b51c68b6aa3cfe75119b551bf4dd1
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (company-dict "1.2.8") (lsp-ui "8.0.1") (preview-it "1.1.0") (define-it "0.2.5") (msgu "0.1.0" ) (elenv "0.1.0" ))
;; Keywords: help

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
;; Generic popup tip
;;

;;; Code:

(require 'elenv)
(require 'msgu)

(require 'company-dict)
(require 'lsp-mode)
(require 'lsp-ui)
(require 'preview-it)
(require 'define-it)  ; this includes `popup', `pos-tip', and `posframe'

(defgroup jcs-poptip nil
  "Generic popup tip."
  :group 'convenience)

(defcustom jcs-poptip-text-scale-level 0
  "Text scale amount for doc buffer."
  :type 'integer
  :group 'jcs-poptip)

(defcustom jcs-poptip-background-color "#2A2D38"
  "Background color string."
  :type 'string
  :group 'jcs-poptip)

(defcustom jcs-poptip-foreground-color "#F1F1F1"
  "Foreground color string."
  :type 'string
  :group 'jcs-poptip)

(defconst jcs-poptip--buffer-name "*jcs-poptip*"
  "Buffer name for posframe tooltip.")

;;
;;; Util

(defun jcs-poptip-2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

;;
;;; Core

(defun jcs-poptip--next ()
  "Hide tooltip after first post command."
  (posframe-hide jcs-poptip--buffer-name)
  (remove-hook 'post-command-hook #'jcs-poptip--next))

(defun jcs-poptip--post ()
  "Register for next post command."
  (add-hook 'post-command-hook #'jcs-poptip--next)
  (remove-hook 'post-command-hook #'jcs-poptip--post))

(cl-defun jcs-poptip-create (string &key point (timeout 300) (height 30))
  "Pop up an tooltip depends on the graphic used.

STRING is the content of the toolip.  The location POINT.  TIMEOUT for not
forever delay.  HEIGHT of the tooltip that will display."
  (let ((bg jcs-poptip-background-color)
        (fg jcs-poptip-foreground-color)
        (fringe-width 10))
    (cond (elenv-graphic-p
           (with-current-buffer (get-buffer-create jcs-poptip--buffer-name)
             (let ((text-scale-mode-step 1.1))
               (text-scale-set jcs-poptip-text-scale-level)))
           (posframe-show jcs-poptip--buffer-name
                          :string string :position point
                          :timeout timeout
                          :background-color bg :foreground-color fg
                          :internal-border-width 1
                          :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                          :left-fringe fringe-width :right-fringe fringe-width)
           (add-hook 'post-command-hook #'jcs-poptip--post))
          (t
           (popup-tip string :point point :around t :height height :scroll-bar t :margin t)))
    t))

(defun jcs-poptip--describe-symbol-string ()
  "Return the describe symbol string."
  (let ((thing (symbol-at-point)))
    (save-window-excursion
      (with-current-buffer (help-buffer)
        (let (buffer-read-only) (erase-buffer))
        (msgu-silent (describe-symbol thing))
        (buffer-string)))))

(defun jcs-poptip--describe-it ()
  "Describe symbol at point."
  (let ((desc (jcs-poptip--describe-symbol-string)))
    (if (or (string-empty-p desc) (string= (string-trim desc) "[back]"))
        (error "[ERROR] No description at point")
      (jcs-poptip-create desc :point (point)))))

(defun jcs-poptip--company-dict ()
  "Describe symbol at point."
  (let* ((thing (jcs-poptip-2str (symbol-at-point)))  ; this has no use
         (dicts (company-dict--relevant-dicts))
         (mem (member thing dicts))                   ; it stores in text property
         (desc (company-dict--quickhelp-string (car mem))))
    (jcs-poptip-create desc :point (point))))

;;;###autoload
(defun jcs-poptip ()
  "Show current symbol info."
  (interactive)
  (company-abort)
  (if (bound-and-true-p lsp-managed-mode)  ; if lsp is connected
      (or (ignore-errors (call-interactively #'lsp-ui-doc-glance))
          (ignore-errors (call-interactively #'lsp-ui-doc-show)))
    (cond ((ignore-errors (jcs-poptip--describe-it)))
          ((ignore-errors (jcs-poptip--company-dict)))
          ((ignore-errors (preview-it)))
          (t (define-it-at-point)))
    ;; In case we are using region, cancel the select region.
    (deactivate-mark)))

(provide 'jcs-poptip)
;;; jcs-poptip.el ends here
