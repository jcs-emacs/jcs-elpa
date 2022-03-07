;;; buffer-menu-filter.el --- Filter buffer-menu items using fake header  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-02-19 15:21:40

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Filter buffer-menu items using fake header.
;; Keyword: buffer menu filter
;; Version: 0.1.0
;; Package-Version: 20220227.1530
;; Package-Commit: 4c91ef9d49c4c79ded3177b1c2dcd0e813abf460
;; Package-Requires: ((emacs "26.1") (buffer-menu-project "0.1.0") (flx "0.6.1") (ht "2.0"))
;; URL: https://github.com/jcs-elpa/buffer-menu-filter

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
;; Filter buffer-menu items using fake header.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'buffer-menu-project)
(require 'flx)
(require 'ht)

(defgroup buffer-menu-filter nil
  "Filter buffer-menu items using fake header."
  :prefix "buffer-menu-filter-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/buffer-menu-filter"))

(defconst buffer-menu-filter--search-title "Search: "
  "Search bar title in `buffer-menu''s buffer.")

(defconst buffer-menu-filter--key-list
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
    "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "`"
    "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" "\\"
    "~" "{" "}" "[" "]" ";" ":" "'" "\"" "," "." "<" ">"
    "/" "?" "|" " ")
  "List of key to bind.")

(defcustom buffer-menu-filter-delay 0.2
  "Filter delay time."
  :type 'float
  :group 'buffer-menu-filter)

(defconst buffer-menu-filter-name "*Buffer List*"
  "Buffer name for *Buffer List*.")

(defvar buffer-menu-filter--first-enter nil
  "Record if fake header already appears.")

(defvar buffer-menu-filter--return-delay nil
  "Record if hit return when display not ready; once it is ready we redo the
action.")

(defvar buffer-menu-filter--score-standard 0
  "Standard score that minimum to reach, or else delete it.
From scale 0 to 100.")

(defvar buffer-menu-filter--done-filtering t
  "Flag to check if done filtering.")

(defvar buffer-menu-filter--timer nil
  "Store filter timer function.")

(defvar buffer-menu-filter--pattern ""
  "Search pattern.")

;;
;; (@* "Entry" )
;;

(defun buffer-menu-filter--major-mode-hook ()
  "Assign this to `Buffer-menu-mode-hook'."
  (setq buffer-menu-filter--first-enter nil)
  (local-set-key (kbd "<return>") #'buffer-menu-filter-return)
  (local-set-key (kbd "<backspace>") #'buffer-menu-filter-backspace)
  (dolist (key-str buffer-menu-filter--key-list)
    (local-set-key key-str (lambda () (interactive) (buffer-menu-filter--input key-str)))))

(defun buffer-menu-filter--list-buffers-noselect (&rest _)
  "Advice around function `list-buffers-noselect'."
  (setq buffer-menu-filter--return-delay nil))

(defun buffer-menu-filter-mode--enable ()
  "Enable function `buffer-menu-filter-mode'."
  (add-hook 'Buffer-menu-mode-hook #'buffer-menu-filter--major-mode-hook)
  (advice-add 'list-buffers-noselect :after #'buffer-menu-filter--list-buffers-noselect))

(defun buffer-menu-filter-mode--disable ()
  "Disable function `buffer-menu-filter-mode'."
  (remove-hook 'Buffer-menu-mode-hook #'buffer-menu-filter--major-mode-hook)
  (advice-remove 'list-buffers-noselect #'buffer-menu-filter--list-buffers-noselect))

;;;###autoload
(define-minor-mode buffer-menu-filter-mode
  "Minor mode 'buffer-menu-filter-mode'."
  :global t
  :require 'buffer-menu-filter-mode
  :group 'buffer-menu-filter
  (if buffer-menu-filter-mode (buffer-menu-filter-mode--enable) (buffer-menu-filter-mode--disable)))

;;
;; (@* "Util" )
;;

(defun buffer-menu-filter-p ()
  "Check if current major mode `buffer-menu'."
  (eq major-mode 'Buffer-menu-mode))

(defun buffer-menu-filter--kill-timer (tmr)
  "Kill timer (TMR) the safe way."
  (when (timerp tmr) (cancel-timer tmr) (setf tmr nil) tmr))

(defun buffer-menu-filter--goto-line (ln)
  "Goto LN line number."
  (goto-char (point-min)) (forward-line (1- ln)))

(defun buffer-menu-filter--window-list (query)
  "Return window list by it's QUERY."
  (cl-remove-if-not
   (lambda (win) (string-match-p query (buffer-name (window-buffer win))))
   (window-list)))

(cl-defun buffer-menu-filter--jump-to-buffer-windows (buffer &key success error)
  "Safely jump to BUFFER's window and execute SUCCESS operations.

If BUFFER isn't showing; then execute ERROR operations instead."
  (if-let ((windows (buffer-menu-filter--window-list buffer)))
      (dolist (win windows)
        (with-selected-window win
          (when success (funcall success))))
    (when error (funcall error))))

;;
;; (@* "Search / Filter" )
;;

(defun buffer-menu-filter-return ()
  "Implemenetation for `buffer menu`'s return key."
  (interactive)
  (if buffer-menu-filter--done-filtering
      (progn
        (ignore-errors (Buffer-menu-this-window))
        (if (buffer-menu-filter-p)
            (user-error "No buffer on this line")
          (message nil)))  ; Use to clear `[Display not ready]'.
    (setq buffer-menu-filter--return-delay t)
    (message "[Display not ready]")))

(defun buffer-menu-filter-backspace ()
  "Backspace."
  (interactive)
  (buffer-menu-filter--input "" -1))


;;;###autoload
(defun buffer-menu-filter-refresh ()
  "Refresh `buffer-menu' table."
  (interactive)
  (tabulated-list-revert)
  (setq tabulated-list--header-string nil)
  (buffer-menu-filter--update-header-string)
  (tabulated-list-print-fake-header))

(defun buffer-menu-filter--safe-print-fake-header ()
  "Safe way to print fake header."
  (unless (tabulated-list-header-overlay-p) (tabulated-list-print-fake-header)))

(defun buffer-menu-filter--clean ()
  "Clean all the menu list."
  (goto-char (point-min))
  (while (< (line-number-at-pos) (line-number-at-pos (point-max)))
    (if (tabulated-list-get-id) (tabulated-list-delete-entry) (forward-line 1))))

(defun buffer-menu-filter--filter-list ()
  "Do filtering the buffer list."
  (buffer-menu-filter--jump-to-buffer-windows
   buffer-menu-filter-name
   :success
   (lambda ()
     (let ((scoring-table (ht-create)) scoring-keys)
       (while (< (line-number-at-pos) (line-number-at-pos (point-max)))
         (let* ((id (tabulated-list-get-id))
                (entry (tabulated-list-get-entry))
                (buf-name (buffer-name id))
                (scoring (flx-score buf-name buffer-menu-filter--pattern))
                ;; Ensure score is not `nil'
                (score (cond ((listp scoring) (nth 0 scoring))
                             ((vectorp scoring) (aref scoring 0))
                             ((numberp scoring) scoring)
                             (t 0))))
           (when score
             (push (cons id entry) (ht-get scoring-table score))))
         (forward-line 1))
       ;; Get all the keys into a list.
       (ht-map (lambda (score-key _) (push score-key scoring-keys)) scoring-table)
       (setq scoring-keys (sort scoring-keys #'>))  ; Sort keys in order
       (buffer-menu-filter--clean)  ; Clean it
       (dolist (key scoring-keys)
         (when (< buffer-menu-filter--score-standard key)
           (let ((ens (sort (ht-get scoring-table key)
                            (lambda (en1 en2)
                              (let ((en1-str (buffer-name (car en1)))
                                    (en2-str (buffer-name (car en2))))
                                (string-lessp en1-str en2-str))))))
             (dolist (en ens)
               (tabulated-list-print-entry (car en) (cdr en))))))
       (buffer-menu-filter--goto-line 2))
     (setq buffer-menu-filter--done-filtering t)
     (buffer-menu-filter--safe-print-fake-header)
     ;; Once it is done filtering, we redo return action if needed.
     (when buffer-menu-filter--return-delay (buffer-menu-filter-return)))))

(defun buffer-menu-filter--update-header-string ()
  "Update the header string."
  (let ((title buffer-menu-filter--search-title))
    (when buffer-menu-project-name
      (setq title (concat "[%s] " title)
            title (format title buffer-menu-project-name)))
    (when (> (length title) (length tabulated-list--header-string))
      (setq-local tabulated-list--header-string title))
    (setq buffer-menu-filter--pattern (substring tabulated-list--header-string
                                                 (length title)
                                                 (length tabulated-list--header-string)))))

(defun buffer-menu-filter--trigger-filter ()
  "Trigger the filtering operation, with PRINT-HEADER."
  (tabulated-list-revert)
  (buffer-menu-filter--update-header-string)
  (buffer-menu-filter--safe-print-fake-header)
  (unless (string-empty-p buffer-menu-filter--pattern)
    (setq buffer-menu-filter--timer (buffer-menu-filter--kill-timer buffer-menu-filter--timer)
          buffer-menu-filter--done-filtering nil
          buffer-menu-filter--timer
          (run-with-idle-timer buffer-menu-filter-delay
                               nil #'buffer-menu-filter--filter-list))))

(defun buffer-menu-filter--input (key-input &optional add-del-num)
  "Insert key KEY-INPUT for fake header for search bar.
ADD-DEL-NUM : Addition or deletion number."
  (unless buffer-menu-filter--first-enter
    (buffer-menu-filter--update-header-string)
    (setq buffer-menu-filter--first-enter t))
  (unless add-del-num (setq add-del-num (length key-input)))
  (if (> add-del-num 0)
      (setq tabulated-list--header-string
            (concat tabulated-list--header-string key-input))
    (setq tabulated-list--header-string
          (substring tabulated-list--header-string 0 (1- (length tabulated-list--header-string)))))
  (buffer-menu-filter--trigger-filter))

(provide 'buffer-menu-filter)
;;; buffer-menu-filter.el ends here
