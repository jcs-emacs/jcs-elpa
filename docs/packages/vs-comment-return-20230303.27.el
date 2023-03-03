;;; vs-comment-return.el --- Comment return like Visual Studio  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/vs-comment-return
;; Package-Version: 20230303.27
;; Package-Commit: e72d87ce55989f8b91c0c6da0d7c1b455b72439e
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
  '()
  "Exclude these comment prefixes."
  :type 'list
  :group 'vs-comment-return)

(defcustom vs-comment-return-keep-suffix nil
  "Do not expand suffix comment symbol."
  :type 'boolean
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
  "Get the before character as the `string'."
  (if (char-before) (string (char-before)) ""))

(defun vs-comment-return--comment-p ()
  "Return non-nil if it's inside comment."
  (nth 4 (syntax-ppss)))

(defun vs-comment-return--goto-start-comment ()
  "Go to the start of the comment."
  (while (vs-comment-return--comment-p)
    (re-search-backward comment-start-skip nil t)))

(defun vs-comment-return--goto-end-comment ()
  "Go to the end of the comment."
  (when (vs-comment-return--comment-p)
    (forward-char 1)
    (vs-comment-return--goto-end-comment)))

(defun vs-comment-return--comment-start-point ()
  "Return comment start point."
  (save-excursion (vs-comment-return--goto-start-comment) (point)))

(defun vs-comment-return--comment-end-point ()
  "Return comment end point."
  (save-excursion (vs-comment-return--goto-end-comment) (point)))

(defun vs-comment-return--multiline-comment-p ()
  "Return non-nil, if current point inside multi-line comment block."
  (let* ((start (vs-comment-return--comment-start-point))
         (end (vs-comment-return--comment-end-point))
         (old-major-mode major-mode)
         (start-point (1+ (- (point) start)))
         (content (buffer-substring start end)))
    (with-temp-buffer
      (insert content)
      (goto-char start-point)
      (insert "\n")
      (delay-mode-hooks (funcall old-major-mode))
      (ignore-errors (font-lock-ensure))
      (vs-comment-return--comment-p))))

(defun vs-comment-return--re-search-forward-end (regexp &optional bound)
  "Repeatedly search REGEXP to BOUND."
  (let ((repeat (1- (length regexp))))
    (while (re-search-forward regexp bound t)
      (backward-char repeat))))  ; Always move backward to search repeatedly!

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
    (while (and (vs-comment-return--comment-p)
                (not (bolp)))
      (backward-char 1))
    (if (re-search-backward "[ \t\n]" (line-beginning-position) t)
        (1+ (point))
      (line-beginning-position))))

(defun vs-comment-return--get-comment-prefix ()
  "Return comment prefix string."
  (save-excursion
    (end-of-line)
    (comment-search-backward (line-beginning-position) t)
    ;; Double check if comment exists
    (unless (= (point) (line-beginning-position))
      (unless (string= (vs-comment-return--before-char-string) " ")
        (unless (re-search-forward "[ \t]" (line-end-position) t)
          (goto-char (line-end-position))))
      (buffer-substring (vs-comment-return--backward-until-not-comment) (point)))))

(defun vs-comment-return--comment-doc-p (prefix)
  "Return non-nil if comment (PREFIX) is a valid document."
  (let ((trimmed (string-trim comment-start)))
    (with-temp-buffer
      (insert prefix)
      (goto-char (point-min))
      (vs-comment-return--re-search-forward-end trimmed (line-end-position))
      (forward-char 1)
      (delete-region (point-min) (point))
      (string-empty-p (string-trim (buffer-string))))))

(defun vs-comment-return--doc-only-line-column (prefix)
  "Return nil there is code interaction within the same line; else we return
the column of the line.

We use PREFIX for navigation; we search it, then check what is infront."
  (save-excursion
    (search-backward prefix (line-beginning-position) t)
    (when (vs-comment-return--infront-first-char-at-line-p)
      (current-column))))

(defun vs-comment-return--next-line-comment-p ()
  "Return non-nil when next line is a comment."
  (save-excursion
    (forward-line 1)
    (end-of-line)
    (vs-comment-return--comment-p)))

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

(defun vs-comment-return--comment-line (prefix &optional column)
  "Insert PREFIX comment with COLUMN for alignment."
  (when column
    (indent-to-column column))
  (insert (string-trim prefix) " "))

(defun vs-comment-return--do-return (func args)
  "Do VS like comment return."
  (cond
   ((not (vs-comment-return--comment-p))
    (apply func args))
   ;; Multi-line comment
   ((vs-comment-return--multiline-comment-p)
    (apply func args)
    (vs-comment-return--c-like-return))
   ;; Single line comment
   (t
    (let* ((prefix (vs-comment-return--get-comment-prefix))
           (doc-only-column (vs-comment-return--doc-only-line-column prefix))
           (empty-comment (vs-comment-return--empty-comment-p prefix))
           (next-ln-comment (vs-comment-return--next-line-comment-p)))
      (apply func args)  ; make return
      (when
          (and doc-only-column
               (vs-comment-return--comment-doc-p prefix)
               (not (member (string-trim prefix) vs-comment-return-exclude-comments))
               (vs-comment-return--current-line-empty-p)
               (or next-ln-comment (not empty-comment)))
        (vs-comment-return--comment-line prefix doc-only-column))))))

;;
;; (@* "C-like" )
;;

(defun vs-comment-return--c-like-multiline-comment-p ()
  "Return non-nil if we are in c-like multiline comment."
  (save-excursion
    (when (comment-search-backward (point-min) t)
      (string-prefix-p "/*" (vs-comment-return--get-comment-prefix)))))

(defun vs-comment-return--c-like-return ()
  "Do C-like comment return for /**/."
  (when (vs-comment-return--c-like-multiline-comment-p)
    (vs-comment-return--comment-line "* ")
    (when (and (not vs-comment-return-keep-suffix)
               (save-excursion (search-forward "*/" (line-end-position) t)))
      (save-excursion
        (insert "\n")
        (indent-for-tab-command)))))

(provide 'vs-comment-return)
;;; vs-comment-return.el ends here
