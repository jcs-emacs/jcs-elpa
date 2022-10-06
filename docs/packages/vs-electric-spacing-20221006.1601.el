;;; vs-electric-spacing.el --- Add spacing around operators like Visual Studio  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/vs-electric-spacing
;; Package-Version: 20221006.1601
;; Package-Commit: d1e4e5cc3180e3137289c188961752c5598b3643
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience electric vs

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
;; Add spacing around operators like Visual Studio.
;;

;;; Code:

(require 'elec-pair)

(defgroup vs-electric-spacing nil
  "Add spacing around operators like Visual Studio."
  :prefix "vs-electric-spacing-"
  :group 'electricity
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/vs-electric-spacing"))


(defun vs-electric-spacing--char-before (&optional pos)
  "Like `char-before' but return a string at POS."
  (ignore-errors (string (char-before pos))))

(defun vs-electric-spacing--char-after (&optional pos)
  "Like `char-after' but return a string at POS."
  (ignore-errors (string (char-after pos))))

(defun vs-electric-spacing--post-self-insert (&rest _)
  "Hook function for `post-self-insert-hook'."
  (when-let* ((b-1 (vs-electric-spacing--char-before (1- (point))))
              (b-0 (vs-electric-spacing--char-before))
              (a-0 (vs-electric-spacing--char-after))
              (a-1 (vs-electric-spacing--char-after (1+ (point)))))
    (pcase b-0
      ("{"
       ;; Insert a space before {
       (unless (string= b-1 " ")
         (save-excursion
           (forward-char -1)
           (insert " ")))
       ;; Insert a space between { }
       (when (string= a-0 "}") (insert " "))
       ;; Insert a space after }
       (when (string= a-1 "}")
         (save-excursion
           (forward-char 1)
           (insert " "))))
      (";"
       ;; Insert a space after ; (if needed)
       (when (and (not (eolp))
                  (string= b-1 " ")
                  (not (string= a-0 " ")))
         (insert " ")
         (forward-char -1))))))

(defun vs-electric-spacing--enable ()
  "Enable `vs-electric-spacing-mode'."
  ;; this require `electric-pair-mode' to be enabled
  (unless electric-pair-mode (electric-pair-mode 1))
  (add-hook 'post-self-insert-hook #'vs-electric-spacing--post-self-insert nil t))

(defun vs-electric-spacing--disable ()
  "Disable `vs-electric-spacing-mode'."
  (remove-hook 'post-self-insert-hook #'vs-electric-spacing--post-self-insert t))

;;;###autoload
(define-minor-mode vs-electric-spacing-mode
  "Minor mode `vs-electric-spacing-mode'."
  :group 'vs-electric-spacing
  (if vs-electric-spacing-mode (vs-electric-spacing--enable) (vs-electric-spacing--disable)))

(defun vs-electric-spacing--turn-on ()
  "Turn on the `vs-electric-spacing-mode'."
  (vs-electric-spacing-mode 1))

;;;###autoload
(define-globalized-minor-mode global-vs-electric-spacing-mode
  vs-electric-spacing-mode vs-electric-spacing--turn-on
  :require 'vs-electric-spacing)

(provide 'vs-electric-spacing)
;;; vs-electric-spacing.el ends here
