;;; electric-indent-sexp.el --- Automatically indent entire balanced expression block  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-02-22 17:10:55

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Automatically indent entire balanced expression block.
;; Keyword: indent sexp electric
;; Version: 0.1.0
;; Package-Version: 20220319.1244
;; Package-Commit: b8b1ec1c381b7a3b572527b9ee7618ef00775116
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/jcs-elpa/electric-indent-sexp

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
;; Automatically indent entire balanced expression block.
;;

;;; Code:

(require 'subr-x)

(defgroup electric-indent-sexp nil
  "Automatically indent entire balanced expression block."
  :prefix "electric-indent-sexp-"
  :group 'electricity
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/electric-indent-sexp"))

(defcustom electric-indent-sexp-chars-alist
  '((actionscript-mode     . (?\n ?\) ?\] ?\}))
    (c-mode                . (?\n ?\) ?\] ?\}))
    (c++-mode              . (?\n ?\) ?\] ?\}))
    (csharp-mode           . (?\n ?\) ?\] ?\}))
    (objc-mode             . (?\n ?\) ?\] ?\}))
    (swift-mode            . (?\n ?\) ?\] ?\}))
    (java-mode             . (?\n ?\) ?\] ?\}))
    (groovy-mode           . (?\n ?\) ?\] ?\}))
    (javascript-mode       . (?\n ?\) ?\] ?\}))
    (js-mode               . (?\n ?\) ?\] ?\}))
    (js2-mode              . (?\n ?\) ?\] ?\}))
    (js3-mode              . (?\n ?\) ?\] ?\}))
    (json-mode             . (?\n ?\) ?\] ?\}))
    (rjsx-mode             . (?\n ?\) ?\] ?\}))
    (typescript-mode       . (?\n ?\) ?\] ?\}))
    (lisp-mode             . (?\n ?\)))
    (lisp-interaction-mode . (?\n ?\)))
    (emacs-lisp-mode       . (?\n ?\)))
    (cask-mode             . (?\n ?\)))
    (eask-mode             . (?\n ?\)))
    (scala-mode            . (?\n ?\) ?\] ?\}))
    (rust-mode             . (?\n ?\) ?\] ?\}))
    (rustic-mode           . (?\n ?\) ?\] ?\}))
    (go-mode               . (?\n ?\) ?\] ?\}))
    (css-mode              . (?\n ?\) ?\] ?\}))
    (scss-mode             . (?\n ?\) ?\] ?\}))
    (php-mode              . (?\n ?\) ?\] ?\}))
    (web-mode              . (?\n ?\) ?\] ?\}))
    (sh-mode               . (?\n ?\) ?\] ?\})))
  "Alist of indent chars to update to each major-mode; (major-mode . chars)."
  :type 'alist
  :group 'electric-indent-sexp)

(defcustom electric-indent-sexp-auto-chars t
  "Automatically update chars for major-mode."
  :type 'boolean
  :group 'electric-indent-sexp)

(defvar electric-indent-sexp--chars-default electric-indent-chars
  "Store default indent characters.")

(defmacro electric-indent-sexp--mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let ((inhibit-message t) message-log-max) ,@body))

(defun electric-indent-sexp--post-self-insert (fn &rest args)
  "Execute around function `electric-indent-post-self-insert-function'."
  (when (apply fn args)
    (electric-indent-sexp--mute-apply
      (when-let ((beg (save-excursion (ignore-errors (backward-sexp)) (point)))
                 (end (point)))
        (unless (= beg end) (indent-region beg end))))))

;;;###autoload
(defun electric-indent-sexp-update-chars ()
  "Update `electric-indent-chars' according to `electric-indent-sexp-chars-alist'."
  (when electric-indent-sexp-auto-chars
    (setq-local electric-indent-chars
                (or (cdr (assq major-mode electric-indent-sexp-chars-alist))
                    electric-indent-sexp--chars-default))))  ; default

(defun electric-indent-sexp--enable ()
  "Enable function `electric-indent-sexp-mode'."
  (advice-add 'electric-indent-post-self-insert-function :around #'electric-indent-sexp--post-self-insert)
  (add-hook 'after-change-major-mode-hook #'electric-indent-sexp-update-chars))

(defun electric-indent-sexp--disable ()
  "Disable function `electric-indent-sexp-mode'."
  (advice-remove 'electric-indent-post-self-insert-function #'electric-indent-sexp--post-self-insert)
  (remove-hook 'after-change-major-mode-hook #'electric-indent-sexp-update-chars))

;;;###autoload
(define-minor-mode electric-indent-sexp-mode
  "Minor mode 'electric-indent-sexp-mode'."
  :global t
  :require 'electric-indent-sexp-mode
  :group 'electric-indent-sexp
  (if electric-indent-sexp-mode (electric-indent-sexp--enable) (electric-indent-sexp--disable)))

(provide 'electric-indent-sexp)
;;; electric-indent-sexp.el ends here
