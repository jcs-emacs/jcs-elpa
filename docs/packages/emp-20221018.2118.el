;;; emp.el --- Emacs Music Playlist  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-03-02 20:54:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/emp
;; Package-Version: 20221018.2118
;; Package-Commit: b7d56702394c6f06dd500273cbd5f7e355165d4d
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (async "1.9.3") (f "0.20.0") (buffer-wrap "0.1.5"))
;; Keywords: multimedia

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

(require 'cl-lib)
(require 'files)
(require 'tabulated-list)

(require 'async)
(require 'buffer-wrap)
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

(defcustom emp-dont-use-unicode nil
  "If non-nil, don't use any unicode in the setup."
  :type 'boolean
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

(defvar emp--volume 20
  "Current play sound volume.")

(defvar emp--mode 'single
  "Current play mode.

This can be one of these value,

  - single
  - single-repeat
  - cycle
  - cycle-repeat
  - random")

(defvar emp--current-path ""
  "Current play music path.")

;;
;; (@* "Entry" )
;;

(defvar emp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-f") #'emp-find-file)
    (define-key map (kbd "<backspace>") #'emp-remove-file)
    (define-key map (kbd "DEL") #'emp-remove-file)
    (define-key map (kbd "RET") #'emp-select-music)
    (define-key map (kbd "<mouse-1>") #'emp-select-music)
    (define-key map (kbd "<space>") #'emp-stop)
    (define-key map (kbd "M-<left>") #'emp-volume-dec)
    (define-key map (kbd "M-<right>") #'emp-volume-inc)
    (define-key map (kbd "l") #'emp-cycle-mode)
    (define-key map (kbd "r") #'emp-replay)
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
              (format "[?] Mode: %s, Volume: %s"
                      (if emp-dont-use-unicode
                          (cl-case emp--mode
                            (`single "Single")
                            (`single-repeat "Single-Repeat")
                            (`cycle "Cycle")
                            (`cycle-repeat "Cycle-Repeat")
                            (`random "Random"))
                        (cl-case emp--mode
                          (`single "➡️")
                          (`single-repeat "🔂")
                          (`cycle "⬇️")
                          (`cycle-repeat "🔃")
                          (`random "🔀")))
                      emp--volume))
  (setq tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (emp--get-entries))
  (tabulated-list-print t)
  (tabulated-list-print-fake-header)
  (buffer-wrap-mode 1))

;;;###autoload
(defun emp ()
  "Start `emp-mode'."
  (interactive)
  (if (get-buffer-window emp--buffer-name)
      (pop-to-buffer emp--buffer-name nil)
    (switch-to-buffer-other-window emp--buffer-name))
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
          emp--mode (plist-get data :mode))))

(defun emp--save-settings ()
  "Save settings file."
  (write-region (emp--2-str (list :volume emp--volume
                                  :mode emp--mode))
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

(defun emp--entry (index)
  "Return entry by INDEX."
  (when (and (<= index (length tabulated-list-entries))
             (<= 0 index))
    (cadr (nth index tabulated-list-entries))))

(defun emp--music-index ()
  "Return current music index."
  (cl-position-if (lambda (entry)
                    (equal emp--current-path (emp--music-file (cadr entry))))
                  tabulated-list-entries))

(defun emp--next-music (loop)
  "Return the filename of the next music.

If the music exceeded the list we return the first music instead, but when
LOOP is nil then we simply return nil."
  (when-let* ((current (emp--music-index))
              (next (1+ (emp--music-index)))
              (next (if (<= (length tabulated-list-entries) next)  ; exceeded
                        (if loop 0 -1)
                      next))
              (entry (emp--entry next)))
    (emp--music-file entry)))

(defun emp--after-play ()
  "Execution after playing a music."
  (when (get-buffer emp--buffer-name)
    (with-current-buffer emp--buffer-name
      (cl-case emp--mode
        (`single )  ; do nothing
        (`single-repeat (emp--play-async emp--current-path emp--volume))
        (`cycle
         (when-let ((next-music (emp--next-music nil)))
           (emp--play-async next-music emp--volume)))
        (`cycle-repeat (emp--play-async (emp--next-music t) emp--volume))
        (`random
         (when-let* ((index (random (length tabulated-list-entries)))
                     (entry (emp--entry index))
                     (random-music (emp--music-file entry)))
           (emp--play-async random-music emp--volume)))))))

(defun emp--play-async (path volume)
  "Async play sound file PATH and with VOLUME."
  (emp-stop)
  (setq emp--current-path path)
  (emp--revert-buffer)
  (setq emp--sound-process
        (async-start
         (lambda (&rest _) (play-sound-file path volume))
         (lambda (&rest _) (emp--after-play)))))

(defun emp-stop ()
  "Stop the sound from current process."
  (interactive)
  (when (processp emp--sound-process)
    (ignore-errors (kill-process emp--sound-process))
    (ignore-errors (kill-buffer (process-buffer emp--sound-process)))
    (setq emp--sound-process nil)
    (emp--revert-buffer)))

(defun emp-replay ()
  "Replay current music."
  (interactive)
  (unless (string-empty-p emp--current-path)
    (emp--play-async emp--current-path emp--volume)
    (emp)))

(defun emp-pause ()
  "Pause the sound process."
  (interactive)
  ;; TODO: ..
  (message "EMP currently doesn't support his functionality"))

(defun emp-resume ()
  "Continue the sound process."
  (interactive)
  ;; TODO: ..
  (message "EMP currently doesn't support his functionality"))

(defun emp--music-file (&optional entry)
  "Return a music filename from current item.

If ENTRY exists, use that instead."
  (let ((entry (or entry (tabulated-list-get-entry))))
    (when (vectorp entry)
      (aref entry 2))))

(defun emp-select-music ()
  "Play sound for current item."
  (interactive)
  (when-let ((path (emp--music-file)))
    (emp--play-async path emp--volume)))

(defun emp-find-file (filename &rest _)
  "Find the music FILENAME."
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
  (emp--save-settings)
  (emp))

(defun emp-volume-inc ()
  "Increase volume."
  (interactive)
  (setq emp--volume (min 100 (+ emp--volume (abs emp-volume-delta))))
  (emp--save-settings)
  (emp))

(defun emp-cycle-mode ()
  "Cycle play mode."
  (interactive)
  (let ((next (memq emp--mode '(single single-repeat cycle cycle-repeat random))))
    (if (= 1 (length next))
        (setq emp--mode 'single)
      (setq emp--mode (nth 1 next))))
  (emp--save-settings)
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
  (emp--load-settings)
  (let (entries)
    (dolist (path emp--paths)
      (push (emp--new-music-entry path) entries))
    entries))

;;
;; (@* "Hooks" )
;;

(defun emp--after-focus-change (&rest _)
  "Run when window got focused in/out."
  (when (frame-focus-state)
    (emp--load-history)
    (emp--load-settings)
    (ignore-errors (emp--revert-buffer))))

(add-function :after after-focus-change-function #'emp--after-focus-change)

(provide 'emp)
;;; emp.el ends here
