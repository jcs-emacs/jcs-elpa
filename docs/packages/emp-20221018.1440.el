;;; emp.el --- Emacs Music Playlist  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-03-02 20:54:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/emp
;; Package-Version: 20221018.1440
;; Package-Commit: 97e3060c3bcfa971b8c6ae957dd54fa1dbd5a89e
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (async "1.9.3") (f "0.20.0"))
;; Keywords: music player playlist table meida

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
;; Emacs Music Playlist.
;;

;;; Code:

(require 'files)
(require 'tabulated-list)

(require 'async)
(require 'f)

(defgroup emp nil
  "Emacs Music Playlist."
  :prefix "emp-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/emp"))

(defcustom emp-play-symbol "*"
  "Symbol to mark playing music."
  :type 'string
  :group 'emp)

(defcustom emp-volume-delta 5
  "Delta value to increase/decrease volume."
  :type 'integer
  :group 'emp)

(defconst emp--history-file (f-join user-emacs-directory "emp" "history")
  "Store playlist history.")

(defconst emp--settings-file (f-join user-emacs-directory "emp" "settings")
  "Store user's preferences settings.")

(defconst emp--buffer-name "*emp*"
  "Name of the EMP buffer.")

(defconst emp--format
  (vector (list "-" 1 t)  ; Playing
          (list "Title" 30 t)
          (list "File" 120 t))
  "Format to assign to `tabulated-list-format' variable.")

(defvar emp--paths nil
  "List of history music path.")

(defvar emp--sound-process nil
  "Process that plays the sound.")

(defvar emp--volume 60
  "Current play sound volume.")

(defvar emp--loop nil
  "Current play sound loop.")

(defvar emp--current-path ""
  "Current play music path.")

;;
;; (@* "Entry" )
;;

(defvar emp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-f") #'emp-find-file)
    (define-key map (kbd "<backspace>") #'emp-remove-file)
    (define-key map (kbd "<delete>") #'emp-remove-file)
    (define-key map (kbd "RET") #'emp-select-music)
    (define-key map (kbd "<mouse-1>") #'emp-select-music)
    (define-key map (kbd "<space>") #'emp-stop-sound)
    (define-key map (kbd "M-<left>") #'emp-volume-dec)
    (define-key map (kbd "M-<right>") #'emp-volume-inc)
    (define-key map (kbd "l") #'emp-toggle-loop)
    map)
  "Keymap for `emp-mode'.")

(define-derived-mode emp-mode tabulated-list-mode
  "emp-mode"
  "Major mode for Emacs Music Playlist."
  :keymap emp-mode-map
  :group 'emp
  (ignore-errors (make-directory (f-dirname emp--history-file)))
  (setq tabulated-list-format emp--format)
  (setq tabulated-list-padding 1)
  (setq-local tabulated-list--header-string
              (format "> Volume: %s, Loop: %s"
                      emp--volume
                      (if emp--loop "On" "Off")))
  (setq tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (emp--get-entries))
  (tabulated-list-print t)
  (tabulated-list-print-fake-header))

;;;###autoload
(defun emp ()
  "Start `emp-mode'."
  (interactive)
  (pop-to-buffer emp--buffer-name nil)
  (emp-mode))

;;
;; (@* "Util" )
;;

(defun emp--list-to-string (lst)
  "Convert LST to string."
  (let ((str ""))
    (dolist (item lst)
      (setq str (concat str item "\n")))
    str))

(defun emp--read-file (path)
  "Read a file from PATH."
  (with-temp-buffer (insert-file-contents path) (buffer-string)))

(defun emp--2-str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

;;
;; (@* "Core" )
;;

(defun emp--load-history ()
  "Load history data."
  (unless (file-exists-p emp--history-file) (emp--save-history))
  (let ((paths (split-string (emp--read-file emp--history-file) "\n" t)))
    ;; Remove path when file no longer exists!
    (setq paths (cl-remove-if-not (lambda (path) (file-exists-p path)) paths))
    (setq emp--paths paths)))

(defun emp--save-history ()
  "Save history data."
  (write-region (emp--list-to-string emp--paths)
                nil
                (expand-file-name emp--history-file)))

(defun emp--load-settings ()
  "Load settings file."
  (unless (file-exists-p emp--settings-file) (emp--save-settings))
  (let* ((pattern (emp--read-file emp--settings-file))
         (data (eval (thing-at-point--read-from-whole-string
                      (concat "'" pattern)))))
    (setq emp--volume (plist-get data :volume)
          emp--loop (plist-get data :loop))))

(defun emp--save-settings ()
  "Save settings file."
  (write-region (emp--2-str (list :volume emp--volume
                                  :loop emp--loop))
                nil
                (expand-file-name emp--settings-file)))

(defun emp--revert-buffer ()
  "Revert `emp-mode' buffer."
  (interactive)
  (if (get-buffer emp--buffer-name)
      (with-current-buffer emp--buffer-name
        (let ((old-pt (point)))
          (setq tabulated-list-entries (emp--get-entries))
          (tabulated-list-revert)
          (tabulated-list-print-fake-header)
          (goto-char old-pt)))
    (error "[ERROR] Can't revert emp buffer if is not inside *emp* buffer list")))

(defun emp--play-sound-async (path volume)
  "Async play sound file PATH and with VOLUME."
  (emp-stop-sound)
  (setq emp--sound-process
        (async-start
         (lambda (&rest _)
           (play-sound-file path volume))
         (lambda (&rest _)
           (when emp--loop
             (emp--play-sound-async path emp--volume))))))

(defun emp-stop-sound ()
  "Stop the sound from current process."
  (interactive)
  (when (processp emp--sound-process)
    (ignore-errors (kill-process emp--sound-process))
    (setq emp--sound-process nil)
    (unless emp--loop
      (setq emp--current-path ""))
    (emp--revert-buffer)))

(defun emp-pause-sound ()
  "Pause the sound process."
  (interactive)
  ;; TODO: ..
  (message "EMP currently doesn't support his functionality"))

(defun emp-resume-sound ()
  "Continue the sound process."
  (interactive)
  ;; TODO: ..
  (message "EMP currently doesn't support his functionality"))

(defun emp--music-file ()
  "Return a music filename from current item."
  (let ((entry (tabulated-list-get-entry)))
    (when (vectorp entry)
      (aref entry 2))))

(defun emp-select-music ()
  "Play sound for current item."
  (interactive)
  (when-let ((path (emp--music-file)))
    (emp--play-sound-async path emp--volume)
    (setq emp--current-path path)
    (emp--revert-buffer)))

(defun emp-find-file (filename &rest _)
  "Find the music file."
  (interactive
   (list (read-file-name "Select music file: " default-directory)))
  (push filename emp--paths)
  (emp--save-history)
  (emp))

(defun emp-remove-file ()
  "Remove a item."
  (interactive)
  (when-let ((path (emp--music-file)))
    (setq emp--paths (cl-remove path emp--paths))
    (emp--save-history)
    (emp)))

(defun emp-volume-dec ()
  "Decrease volume."
  (interactive)
  (setq emp--volume (max 0 (- emp--volume (abs emp-volume-delta))))

  (emp))

(defun emp-volume-inc ()
  "Increase volume."
  (interactive)
  (setq emp--volume (min 100 (+ emp--volume (abs emp-volume-delta))))
  (emp))

(defun emp-toggle-loop ()
  "Toggle loop flag."
  (interactive)
  (setq emp--loop (not emp--loop))
  (emp))

;;
;; (@* "Tabulated List" )
;;

(defun emp--new-music-entry (path)
  "Add a music by PATH."
  (let ((id (length tabulated-list-entries))
        new-entry new-entry-value)
    (push path new-entry-value)  ; Path
    (push (f-filename path) new-entry-value)  ; Title
    (if (string= path emp--current-path)  ; PD
        (push emp-play-symbol new-entry-value)
      (push "" new-entry-value))
    (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
    (push (number-to-string id) new-entry)  ; ID
    new-entry))

(defun emp--get-entries ()
  "Get all the music entries."
  (emp--load-history)
  (let (entries)
    (dolist (path emp--paths)
      (push (emp--new-music-entry path) entries))
    entries))

(provide 'emp)
;;; emp.el ends here
