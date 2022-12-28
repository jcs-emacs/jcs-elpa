;;; eldoc-eask.el --- Eldoc support for Eask-file  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-eask/eldoc-eask
;; Package-Version: 20221228.1037
;; Package-Commit: e9019b5187d1c459aa0bc095bc1eac86e7557060
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (eask-api "0.1.0"))
;; Keywords: convenience

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
;; Eldoc support for Eask-file
;;

;;; Code:

(require 'eldoc)

(require 'eask-api-core)

(defgroup eldoc-eask nil
  "Eldoc support for Eask-file."
  :prefix "eldoc-eask-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-eask/eldoc-eask"))

;;
;; (@* "Core" )
;;

(defun eldoc-eask--fnsym-in-current-sexp ()
  "Mainly copy it from `elisp--fnsym-in-current-sexp' function."
  (save-excursion
    (unless (nth 8 (syntax-ppss))
      (let ((argument-index (1- (elisp--beginning-of-sexp))))
        ;; If we are at the beginning of function name, this will be -1.
        (when (< argument-index 0)
          (setq argument-index 0))
        (list (or (elisp--current-symbol)
                  (thing-at-point 'symbol))
              argument-index)))))

(defun eldoc-eask--funcall (callback &rest _ignored)
  "Document function call (CALLBACK) at point.

Mainly copy it from `elisp-eldoc-funcall' function."
  (when-let* ((sym-info (eldoc-eask--fnsym-in-current-sexp))
              (fn-sym (car sym-info))
              ((member (eask-2str fn-sym) eask-file-keywords)))
    (setf (car sym-info) (intern (format "eask-f-%s" fn-sym)))
    (setq fn-sym (car sym-info))
    (funcall callback (apply #'elisp-get-fnsym-args-string sym-info)
             :thing fn-sym
             :face 'font-lock-keyword-face)))

(defun eldoc-eask--function ()
  "Main eldoc entry.

Mainly copy it from `elisp-eldoc-documentation-function' function."
  (let* (str
         (callback (lambda (doc &rest plist)
                     (when doc
                       (setq str
                             (format "%s: %s"
                                     (eask-s-replace
                                      "eask-f-" ""
                                      (propertize (prin1-to-string
                                                   (plist-get plist :thing))
                                                  'face (plist-get plist :face)))
                                     doc))))))
    (or (progn (eldoc-eask--funcall callback) str))))

(defun eldoc-eask--turn-on ()
  "Start the `eldoc-eask' worker."
  (add-function :before-until (local 'eldoc-documentation-function) #'eldoc-eask--function)
  (eldoc-mode 1))

;;
;; (@* "Entry" )
;;

;;;###autoload
(defun eldoc-eask-enable ()
  "Turn on `eldoc-eask'."
  (interactive)
  (add-hook 'eask-mode-hook #'eldoc-eask--turn-on)
  (eldoc-eask--turn-on))

;;;###autoload
(defun eldoc-eask-disable ()
  "Turn off `eldoc-eask'."
  (interactive)
  (remove-hook 'eask-mode-hook #'eldoc-eask--turn-on))

(provide 'eldoc-eask)
;;; eldoc-eask.el ends here
