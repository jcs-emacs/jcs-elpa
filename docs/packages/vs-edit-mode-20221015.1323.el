;;; vs-edit-mode.el --- Minor mode accomplish editing experience in Visual Studio  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-03-11 22:10:58

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/vs-edit-mode
;; Package-Version: 20221015.1323
;; Package-Commit: ce1d66d311359c10c420a9ef0825fa144d5b8799
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience editing vs

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
;; Minor mode accomplish editing experience in Visual Studio.
;;

;;; Code:

(defgroup vs-edit nil
  "Minor mode accomplish editing experience in Visual Studio."
  :prefix "vs-edit-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/vs-edit-mode"))

(defcustom vs-edit-active-modes
  '(actionscript-mode
    c-mode c++-mode csharp-mode objc-mode
    css-mode
    haxe-mode
    java-mode
    javascript-mode js-mode js2-mode js3-mode
    json-mode
    perl-mode
    rjsx-mode
    rust-mode
    shell-mode
    typescript-mode)
  "List of major mode to active minor mode, `vs-edit-mode'."
  :type 'list
  :group 'vs-edit)

;;
;; (@* "Entry" )
;;

(defvar vs-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "{") #'vs-edit-opening-curly-bracket-key)
    (define-key map (kbd ";") #'vs-edit-semicolon-key)
    (define-key map (kbd "#") #'vs-edit-sharp-key)
    (define-key map (kbd "<up>") #'vs-edit-previous-line)
    (define-key map (kbd "<down>") #'vs-edit-next-line)
    map)
  "Keymap used in function `vs-edit-mode'.")

;;;###autoload
(define-minor-mode vs-edit-mode
  "Minor mode `vs-edit-mode'."
  :lighter " VS-Edit"
  :group vs-edit
  (when vs-edit-mode
    (unless (memq major-mode vs-edit-active-modes)
      (vs-edit-mode -1))))

(defun vs-edit-turn-on-mode ()
  "Turn on the `vs-edit-mode'."
  (vs-edit-mode 1))

;;;###autoload
(define-globalized-minor-mode global-vs-edit-mode
  vs-edit-mode vs-edit-turn-on-mode
  :group 'vs-edit
  :require 'vs-edit)

;;
;; (@* "Util" )
;;

(defun vs-edit--comment-or-string-p ()
  "Return non-nil if it's inside comment or string."
  (or (nth 4 (syntax-ppss)) (nth 8 (syntax-ppss))))

(defun vs-edit--delete-region ()
  "Delete region by default value."
  (when (use-region-p) (delete-region (region-beginning) (region-end))))

(defun vs-edit--current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there.  (not absolute)."
  (save-excursion (beginning-of-line) (looking-at "[[:space:]\t]*$")))

(defun vs-edit--current-char-string ()
  "Get the current character as the `string'."
  (if (char-before) (string (char-before)) ""))

(defun vs-edit--current-char-equal-p (c)
  "Check the current character equal to C, C can be a list of character."
  (cond ((and (stringp c) (stringp (vs-edit--current-char-string)))
         (string= (vs-edit--current-char-string) c))
        ((listp c) (member (vs-edit--current-char-string) c))))

(defun vs-edit--current-whitespace-or-tab-p ()
  "Check if current character a whitespace or a tab character?"
  (vs-edit--current-char-equal-p '(" " "\t")))

(defun vs-edit--infront-first-char-at-line-p (&optional pt)
  "Return non-nil if there is nothing infront of the right from the PT."
  (save-excursion
    (when pt (goto-char pt))
    (null (re-search-backward "[^ \t]" (line-beginning-position) t))))

;;
;; (@* "Core" )
;;

(defun vs-edit-opening-curly-bracket-key ()
  "For programming langauge that need `{`."
  (interactive)
  (vs-edit--delete-region)
  (if (vs-edit--comment-or-string-p)
      (insert "{")
    (let (pretty-it space-infront)
      (unless (vs-edit--current-char-equal-p "{")
        (setq pretty-it t)
        (when (and (not (vs-edit--current-whitespace-or-tab-p))
                   (not (vs-edit--current-char-equal-p '("(" "["))))
          (setq space-infront t)))

      (when space-infront (insert " "))

      (insert "{ }")
      (backward-char 1)
      (indent-for-tab-command)

      (when pretty-it
        (save-excursion
          (forward-char 2)
          (when (and (not (eobp))
                     (not (bolp))
                     (vs-edit--current-char-equal-p "}"))
            (backward-char 1)
            (insert " ")))))))

(defun vs-edit-semicolon-key ()
  "For programming language that use semicolon as the end operator sign."
  (interactive)
  (vs-edit--delete-region)
  (insert ";")
  (save-excursion
    (forward-char 1)
    (when (and (not (bolp))
               (vs-edit--current-char-equal-p "}"))
      (backward-char 1)
      (insert " "))))

(defun vs-edit-sharp-key ()
  "For programming language that use # as the preprocessor."
  (interactive)
  (vs-edit--delete-region)
  (insert "#")
  (backward-char 1)
  (when (vs-edit--infront-first-char-at-line-p)
    (kill-region (line-beginning-position) (point)))
  (forward-char 1))

;;
;; (@* "Navigation" )
;;

(defun vs-edit--after-move-line ()
  "Do stuff after smart move line."
  (cond ((vs-edit--current-line-empty-p) (end-of-line))
        ((and (vs-edit--infront-first-char-at-line-p)
              (re-search-forward "[^[:space:]\t]" (line-end-position) t))
         (forward-char -1))))

;;;###autoload
(defun vs-edit-previous-line ()
  "Smart way to navigate to previous line."
  (interactive)
  (call-interactively #'previous-line)
  (vs-edit--after-move-line))

;;;###autoload
(defun vs-edit-next-line ()
  "Smart way to navigate to next line."
  (interactive)
  (call-interactively #'next-line)
  (vs-edit--after-move-line))

(provide 'vs-edit-mode)
;;; vs-edit-mode.el ends here
