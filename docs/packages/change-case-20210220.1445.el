;;; change-case.el --- Case conversion between camelCase, PascalCase, snake_case and more -*- lexical-binding: t -*-

;; Copyright (C) 2020 TakesxiSximada

;; Author: TakesxiSximada <8707279+TakesxiSximada@users.noreply.github.com>
;; Maintainer: TakesxiSximada <8707279+TakesxiSximada@users.noreply.github.com>
;; Repository: https://gist.github.com/TakesxiSximada/e8a10244aac6308de1323d1f6685658b
;; Version: 9
;; Package-Commit: daa82dea863c168e9088a20b6f3e2df1e2a2f40c
;; Package-Version: 20210220.1445
;; Package-X-Original-Version: 20210220.0000
;; Package-Requires: ((emacs "25.1") (dash "2.16.0") (s "1.12.0"))
;; Date: 2021-02-20

;; This package is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; The change-case.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with change-case.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Transform a string between camelCase, PascalCase, snake_case, kebab-case, doted.case
;; and others by Emacs Lisp.

;;; Code:
(require 'rx)

(require 'dash)
(require 's)


;;; Options
(defgroup change-case nil
  "Case conversion."
  :prefix "change-case")

;;; dotted.case
(defvar change-case-dotted-case-separator "."
  "Used as delimiter in doted case.")

(defun change-case-dotted-case-parse (sentence)
  (s-split (regexp-quote change-case-dotted-case-separator)
	   sentence))

(defun change-case-dotted-case-render (word-list)
  (string-join word-list change-case-dotted-case-separator))

;;; path/case
(defvar change-case-path-case-separator "/"
  "Used as delimiter in path case.")

(defun change-case-path-case-parse (sentence)
  (s-split change-case-path-case-separator sentence))

(defun change-case-path-case-render (word-list)
  (string-join word-list change-case-path-case-separator))

;;; snake_case
(defvar change-case-snake-case-separator "_"
  "Used as delimiter in snake case.")

(defun change-case-snake-case-parse (sentence)
  (s-split change-case-snake-case-separator sentence))

(defun change-case-snake-case-render (word-list)
  (string-join (mapcar 'downcase word-list)
	       change-case-snake-case-separator))

;;; kebab-case
(defvar change-case-kebab-case-separator "-"
  "Used as delimiter in kebab case.")

(defun change-case-kebab-case-parse (sentence)
  (s-split change-case-kebab-case-separator sentence))

(defun change-case-kebab-case-render (word-list)
  (string-join (mapcar 'downcase word-list) change-case-kebab-case-separator))

;;; PascalCase
(defun change-case--get-pascal-case-inner-partition (sentence &optional start)
  (let ((case-fold-search nil))
    (if-let ((pos (string-match (rx upper-case) sentence (or start 1))))
	(cons pos
	      (change-case--get-pascal-case-inner-partition sentence (+ pos 1))))))

(defun change-case--get-pascal-case-partition (sentence &optional start)
  (cons 0
	(append (change-case--get-pascal-case-inner-partition sentence start)
		(cons (length sentence) nil))))

(defun change-case--get-pair-list (sequence)
  (if-let ((start (nth 0 sequence))
	   (end (nth 1 sequence)))
      (cons
       (cons start end)
       (change-case--get-pair-list (cdr sequence)))))

(defun change-case-pascal-case-parse (sentence)
  (mapcar
   (lambda (pair) (substring sentence
			     (car pair)
			     (cdr pair)))
   (change-case--get-pair-list
    (change-case--get-pascal-case-partition sentence))))

(defun change-case-pascal-case-render (word-list)
  (string-join (mapcar 'capitalize word-list)))

;;; camelCase
(defun change-case-camel-case-parse (sentence)
  (change-case-pascal-case-parse sentence))

(defun change-case-camel-case-render (word-list)
  (concat
   (downcase (car word-list))
   (change-case-pascal-case-render (cdr word-list))))

;;; Options
(defvar change-case-parser-alist
  '(("dotted.case" . change-case-dotted-case-parse)
    ("path/case" . change-case-path-case-parse)
    ("snake_case" . change-case-snake-case-parse)
    ("kebab-case" . change-case-kebab-case-parse)
    ("PascalCase" . change-case-pascal-case-parse)
    ("camelCase" . change-case-camel-case-parse))
  "List of valid parser functions.")

(defvar change-case-renderer-alist
  '(("dotted.case" . change-case-dotted-case-render)
    ("path/case" . change-case-path-case-render)
    ("snake_case" . change-case-snake-case-render)
    ("kebab-case" . change-case-kebab-case-render)
    ("PascalCase" . change-case-pascal-case-render)
    ("camelCase" . change-case-camel-case-render))
  "List of valid render functions.")

;;; U/I
(defcustom change-case-parser-prompt "change-case: parser:"
  "Prompt used in the parser function selection UI."
  :group 'change-case)

(defcustom change-case-renderer-prompt "change-case: renderer:"
  "Prompt used in the render function selection UI."
  :group 'change-case)

(defcustom change-case-parser-default "dotted.case"
  "Default parser."
  :group 'change-case)

(defcustom change-case-renderer-default "dotted.case"
  "Default renderer."
  :group 'change-case)

(defun change-case-select-ui (prompt choices default)
  (ido-completing-read prompt
		       choices
		       nil nil nil nil
		       default))

(defun change-case-select-parser ()
  (cdr (assoc (change-case-select-ui change-case-parser-prompt
				     (mapcar 'car change-case-parser-alist)
				     change-case-parser-default)
	      change-case-parser-alist)))

(defun change-case-select-renderer ()
  (cdr (assoc (change-case-select-ui change-case-renderer-prompt
				     (mapcar 'car change-case-renderer-alist)
				     change-case-renderer-default)
	      change-case-renderer-alist)))

(defun change-case-edit (start end sentence)
  (delete-region start end)
  (save-excursion
    (goto-char start)
    (insert sentence)))

(defun change-case-update-parser-and-renderer-default (parser renderer)
  (setq change-case-parser-default (car (rassoc parser change-case-parser-alist)))
  (setq change-case-renderer-default (car (rassoc renderer change-case-renderer-alist))))


;;;###autoload
(defun change-case (&optional start end)
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (if-let (bounds (bounds-of-thing-at-point 'symbol))
		     (list (car bounds) (cdr bounds))
		   (list (point-min (point-max))))))

  (let ((sentence (buffer-substring-no-properties start end))
	(parser (change-case-select-parser))
	(renderer (change-case-select-renderer)))
    (change-case-edit
     start end
     (funcall renderer (funcall parser sentence)))
    (change-case-update-parser-and-renderer-default parser
						    renderer)))

;;; _
(provide 'change-case)
;;; change-case.el ends here
