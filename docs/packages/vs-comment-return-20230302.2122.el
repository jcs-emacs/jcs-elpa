;;; vs-comment-return.el --- Comment return like Visual Studio  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/vs-comment-return
;; Package-Version: 20230302.2122
;; Package-Commit: 81cd1fbd5d95047cb5f0d354269c9defb0fb414a
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
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
;; Comment return like Visual Studio.
;;

;;; Code:

(defgroup vs-comment-return nil
  "Comment return like Visual Studio."
  :prefix "vs-comment-return-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/vs-comment-return"))

(defcustom vs-comment-return-exclude-comments
  '("//")
  "Exclude these comment prefixes."
  :type 'list
  :group 'vs-comment-return)

;;
;; (@* "Entry" )
;;

(defun vs-comment-return-mode--enable ()
  "Enable `vs-comment-return' in current buffer."
  (advice-add (key-binding (kbd "RET")) :around #'vs-comment-return--advice-around))

(defun vs-comment-return-mode--disable ()
  "Disable `vs-comment-return' in current buffer."
  (advice-remove (key-binding (kbd "RET")) #'vs-comment-return--advice-around))

;;;###autoload
(define-minor-mode vs-comment-return-mode
  "Minor mode `vs-comment-return-mode'."
  :lighter " VS-ComRet"
  :group vs-comment-return
  (if vs-comment-return-mode (vs-comment-return-mode--enable)
    (vs-comment-return-mode--disable)))

;;
;; (@* "Util" )
;;

(defun vs-comment-return--before-char-string ()
  "Get the before character as the 'string'."
  (if (char-before) (string (char-before)) ""))

(defun vs-comment-return--current-point-face (in-face &optional pos)
  "Check if current POS's face the same face as IN-FACE."
  (let ((faces (jcs-get-current-point-face pos)))
    (cond ((listp faces)
           (if (listp in-face)
               (cl-some (lambda (fc) (cl-position fc faces :test 'string=)) in-face)
             (cl-position in-face faces :test 'string=)))
          (t (string= in-face faces)))))

(defun vs-comment-return--inside-comment-p ()
  "Return non-nil if it's inside comment."
  (nth 4 (syntax-ppss)))

(defun vs-comment-return--current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there.  (not absolute)."
  (save-excursion (beginning-of-line) (looking-at "[[:space:]\t]*$")))

(defun vs-comment-return--infront-first-char-at-line-p (&optional pt)
  "Return non-nil if there is nothing infront of the right from the PT."
  (save-excursion
    (when pt (goto-char pt))
    (null (re-search-backward "[^ \t]" (line-beginning-position) t))))

;;
;; (@* "Core" )
;;

(defun vs-comment-return--backward-until-not-comment ()
  "Move backward to the point when it's not comment."
  (save-excursion
    (while (and (vs-comment-return--inside-comment-p)
                (not (bolp)))
      (backward-char 1))
    (1- (point))))

(defun vs-comment-return--get-comment-prefix ()
  "Return comment prefix string."
  (save-excursion
    (end-of-line)
    (comment-search-backward (line-beginning-position) t)
    ;; Double check if comment exists
    (unless (= (point) (line-beginning-position))
      (unless (string= (vs-comment-return--before-char-string) " ")
        (search-forward " " (line-end-position) t))
      (buffer-substring (vs-comment-return--backward-until-not-comment) (point)))))

(defun vs-comment-return--next-line-comment-p ()
  "Return non-nil when next line is a comment."
  (save-excursion
    (forward-line 1)
    (end-of-line)
    (vs-comment-return--inside-comment-p)))

(defun vs-comment-return--empty-comment-p (prefix)
  "Return non-nil if current line comment is empty (PREFIX only)."
  (let* ((line (thing-at-point 'line))
         (line (string-trim line))
         (content (string-replace (string-trim prefix) "" line)))
    (string-empty-p (string-trim content))))

(defun vs-comment-return--advice-around (func &rest args)
  "Advice bind around return."
  (if (not vs-comment-return-mode)
      (apply func args)
    (vs-comment-return--do-return func args)))

(defun vs-comment-return--comment-line (prefix)
  "Insert PREFIX comment."
  (unless (bolp)
    (indent-for-tab-command))
  (insert (string-trim prefix) " "))

(defun vs-comment-return--do-return (func args)
  "Do VS like comment return."
  (if (vs-comment-return--inside-comment-p)
      (let* ((prefix (vs-comment-return--get-comment-prefix))
             (empty-comment (vs-comment-return--empty-comment-p prefix))
             (next-ln-comment (vs-comment-return--next-line-comment-p)))
        (apply func args)  ; make return
        (when (and
               (not (member (string-trim prefix) vs-comment-return-exclude-comments))
               (vs-comment-return--current-line-empty-p)
               (or next-ln-comment (not empty-comment)))
          (vs-comment-return--comment-line prefix)))
    (apply func args)))  ; make return

(provide 'vs-comment-return)
;;; vs-comment-return.el ends here
