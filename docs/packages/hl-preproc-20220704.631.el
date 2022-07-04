;;; hl-preproc.el --- Unhighlight invalid preprocessor region  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Shen, Jen-Chieh
;; Created date 2021-07-03 17:07:11

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/hl-preproc
;; Package-Version: 20220704.631
;; Package-Commit: bc6a52a626b4735010e56fec8d31c8d1f581a2cf
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (meta-net "1.1.0"))
;; Keywords: convenience preprocessor

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
;; Unhighlight invalid preprocessor region
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'meta-net)

(defgroup hl-preproc nil
  "Unhighlight invalid preprocessor region."
  :prefix "hl-preproc-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/hl-preproc"))

(defcustom hl-preproc-delay 0.4
  "Delay to highlight invalid preprocessor region."
  :type 'float
  :group 'hl-preproc)

(defcustom hl-preproc-priority 150
  "The priority of the overlay used to indicate matches."
  :type 'integer
  :group 'hl-preproc)

(defcustom hl-preproc-extra-constants
  '("true" "TRUE")
  "List of string, extra define constants."
  :type 'list
  :group 'hl-preproc)

(defvar hl-preproc--supported-modes
  '(csharp-mode csharp-tree-sitter-mode)
  "List of current supported major modes.")

(defvar-local hl-preproc--overlays '()
  "List of invalid region overlays.")

(defvar-local hl-preproc--idle-timer nil
  "Delay timer to do unhighlighting.")

(defvar-local hl-preproc--define-constants nil
  "Cache that records all define constants.")

;;
;; (@* "Faces" )
;;

(defface hl-preproc-face
  '((((background light)) :foreground "#808080")
    (t :foreground "#9B9B9B"))
  "The face that overlays the invalid preprocessor area."
  :group 'hl-preproc)

;;
;; (@* "Constants" )
;;

(defun hl-preproc-all-constants (&optional refresh)
  "Return all define constants.

If REFRESH is non-nil, refresh cache once."
  (when (or refresh (null hl-preproc--define-constants))
    (setq hl-preproc--define-constants
          (append hl-preproc-extra-constants (meta-net-define-constants meta-net-csproj-current)))
    (cl-delete-duplicates hl-preproc--define-constants))
  hl-preproc--define-constants)

;;
;; (@* "Core" )
;;

(defun hl-preproc--define-check (expression)
  "Return non-nil if EXPRESSION is defined."
  (let ((constants (hl-preproc-all-constants)))
    (cl-some
     (lambda (constant)
       ;; TODO: This need to improve for a complex expression, especially
       ;; the nested logic. Right now, this will work with direct
       ;; C# directives expression.
       ;;
       ;; > What's working?
       ;;
       ;;   - DEBUG
       ;;   - (DEBUG)
       ;;   - !DEBUG
       ;;   - !(DEBUG)
       ;;   - DEBUG || !(DEBUG)
       ;;
       ;;  > What's not working?
       ;;
       ;;   - ((DEBUG || !(DEBUG)) && DEBUG)
       ;;
       ;; Basically, everything without nested logic should work!
       (and (string-match-p (format "\\_<%s\\_>" constant) expression)
            (not (string-match-p (format "![ \t(]*\\_<%s\\_>" constant) expression))))
     constants)))

(defun hl-preproc--overlay (beg end)
  "Place invalid overlay from BEG to END."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'hl-preproc-face)
    (overlay-put ov 'priority hl-preproc-priority)
    (push ov hl-preproc--overlays)))

(defun hl-preproc--keyword (direc)
  "Return a regular expression to search for DIREC."
  (format "^[ \t]*#[ \t]*%s" direc))

(defun hl-preproc--search-directives (direc)
  "Search DIREC in current buffer and return it's point."
  (save-excursion (re-search-forward (hl-preproc--keyword direc) nil t)))

(defun hl-preproc--next-starting-directives ()
  "Return information about starting directives in (directives . point)."
  (let (direc pt d-if d-elif d-else)
    (setq d-if (hl-preproc--search-directives "if")
          d-elif (hl-preproc--search-directives "elif")
          d-else (hl-preproc--search-directives "else"))
    (setq pt (min (or d-if (point-max)) (or d-elif (point-max)) (or d-else (point-max))))
    (setq direc (cond ((equal pt d-if) 't-if)
                      ((equal pt d-elif) 't-elif)
                      ((equal pt d-else) 't-else)))
    (cons direc pt)))

(defun hl-preproc--next-constant-region ()
  "Return a cons cell of (expression . (beg end))."
  (let* ((starting (hl-preproc--next-starting-directives))
         (start-direc (car starting)) end-direc
         (starting-pt (cdr starting))
         expression beg end p-elif p-else p-endif)
    (unless (= (point-max) starting-pt)
      (goto-char starting-pt)
      (setq beg (1+ (line-end-position))  ; to next line
            expression (buffer-substring (point) beg)
            expression (string-trim expression)
            p-elif (hl-preproc--search-directives "elif")
            p-else (hl-preproc--search-directives "else")
            p-endif (hl-preproc--search-directives "endif")
            end (min (or p-elif (point-max)) (or p-else (point-max)) (or p-endif (point-max)))
            end-direc (cond ((equal end p-elif) 't-elif)
                            ((equal end p-else) 't-else)
                            ((equal end p-endif) 't-endif))
            end (save-excursion (goto-char end) (1- (line-beginning-position)))))
    (when expression (cons (list start-direc end-direc) (list expression beg end)))))

(defun hl-preproc--do-highlight (buffer)
  "Highlight BUFFER with overlays."
  (unless meta-net-csproj-current (meta-net-read-project))  ; read define constants
  (mapc #'delete-overlay hl-preproc--overlays)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (region start-direc end-direc expression beg end last-true)
        (while (progn (setq region (hl-preproc--next-constant-region)) region)
          (setq start-direc (nth 0 (car region)) end-direc (nth 1 (car region))
                expression (nth 0 (cdr region))
                beg (nth 1 (cdr region)) end (nth 2 (cdr region)))
          (if (and (hl-preproc--define-check expression) (not last-true))
              (setq last-true t)
            (cl-case start-direc
              (t-if (hl-preproc--overlay beg end))
              (t-elif (hl-preproc--overlay beg end))
              (t-else (when last-true (hl-preproc--overlay beg end))
                      (setq last-true nil))))
          (when (eq end-direc 't-endif) (setq last-true nil)))))))

(defun hl-preproc--after-cahnge (&rest _)
  "Unhighlight after change."
  (when (timerp hl-preproc--idle-timer) (cancel-timer hl-preproc--idle-timer))
  (setq hl-preproc--idle-timer
        (run-with-idle-timer hl-preproc-delay nil
                             #'hl-preproc--do-highlight (current-buffer))))

(defun hl-preproc--enable ()
  "Start function `hl-preproc-mode'."
  (if (memq major-mode hl-preproc--supported-modes)
      (progn
        (hl-preproc--do-highlight (current-buffer))  ; highlight once kimmediately after activation
        (add-hook 'after-change-functions #'hl-preproc--after-cahnge nil t))
    (user-error "Hl-Preproc doesn't support current major-mode: %s" major-mode)
    (hl-preproc-mode -1)))

(defun hl-preproc--disable ()
  "Stop function `hl-preproc-mode'."
  (mapc #'delete-overlay hl-preproc--overlays)
  (remove-hook 'after-change-functions #'hl-preproc--after-cahnge t))

;;;###autoload
(define-minor-mode hl-preproc-mode
  "Minor mode 'hl-preproc-mode'."
  :lighter " HL-PREPROC"
  :group hl-preproc
  (if hl-preproc-mode (hl-preproc--enable) (hl-preproc--disable)))

(provide 'hl-preproc)
;;; hl-preproc.el ends here
