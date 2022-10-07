;;; execrun.el --- Run through `compilation-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/execrun
;; Package-Version: 20221007.2132
;; Package-Commit: 7e96a162c03838776bd9a4d432475c0df639a636
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (f "0.20.0"))
;; Keywords: tools

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
;; Run through `compilation-mode'.
;;

;;; Code:

(require 'cl-lib)

(require 'f)

(defgroup execrun nil
  "Run through `compilation-mode'."
  :prefix "execrun-"
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/execrun"))

(defconst execrun-script-extension
  (if (memq system-type '(cygwin windows-nt ms-dos)) "[.]bat" "[.]sh")
  "Script file extension in RegExp.")

(defcustom execrun-build-script
  (concat "[[:ascii:]]*build[[:ascii:]]*" execrun-script-extension)
  "Name of the build/make file script."
  :type 'string
  :group 'execrun)

(defcustom execrun-run-script
  (concat "[[:ascii:]]*run[[:ascii:]]*" execrun-script-extension)
  "Name of the execute/run file script."
  :type 'string
  :group 'execrun)

(defcustom execrun-kill-buffer-function #'kill-this-buffer
  "Function to kill output buffer."
  :type 'function
  :group 'execrun)

;;
;; (@* "Entry" )
;;

(defvar execrun-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-k") #'execrun-maybe-kill-buffer)
    (define-key map (kbd "C-_") #'execrun-previous)
    (define-key map (kbd "C-+") #'execrun-next)
    map)
  "Keymap for `execrun-mode'.")

(defcustom execrun-mode-hook nil
  "Hook called when execrun minor mode is activated or deactivated."
  :type 'hook
  :group 'execrun)

;;;###autoload
(define-minor-mode execrun-mode
  "Minor mode for build/run script text in the current compilation buffer."
  :group 'execrun
  :lighter nil
  :keymap execrun-mode-map
  :init-value nil)

;;
;; (@* "Util" )
;;

(defun execrun--get-buffers (str type)
  "Return a list of buffers that match STR.

TYPE is the return type; can be `object or `string."
  (execrun--get-buffers-regexp (regexp-quote str) type))

(defun execrun--get-buffers-regexp (regexp type)
  "Return a list of buffers that match REGEXP.

TYPE is the return type; can be `object or `string."
  (let (buf-lst buf-name)
    (if (not (stringp regexp))
        (user-error "[WARNING] Can't get buffers with this string/regexp: %s" regexp)
      (dolist (buf (buffer-list))
        (setq buf-name (buffer-name buf))
        (when (and (stringp buf-name) (string-match-p regexp buf-name))
          (cl-case type
            (`object (push buf buf-lst))
            (`string (push buf-name buf-lst))))))
    buf-lst))

(defun execrun--string-compare-p (regexp str type &optional ignore-case)
  "Compare STR with REGEXP by TYPE.

Argument TYPE can be on of the following symbol.

  * regex - uses function `string-match-p'.  (default)
  * strict - uses function `string='.
  * prefix - uses function `string-prefix-p'.
  * suffix - uses function `string-suffix-p'.

Optional argument IGNORE-CASE is only uses when TYPE is either symbol `prefix'
or `suffix'."
  (cl-case type
    (`strict (string= regexp str))
    (`prefix (string-prefix-p regexp str ignore-case))
    (`suffix (string-suffix-p regexp str ignore-case))
    (t (ignore-errors (string-match-p regexp str)))))

(defun execrun--buffer-filter (name &optional type)
  "Return a list of buffers with NAME.

See function `execrun--string-compare-p' for argument TYPE."
  (let (lst)
    (dolist (buf (buffer-list))
      (when (execrun--string-compare-p name (buffer-name buf) type)
        (push buf lst)))
    lst))

;;
;; (@* "Control Output" )
;;

(defun execrun-list-compilation ()
  "Return the list of compilation buffers."
  (execrun--buffer-filter (format "[*]%s[*]: " execrun-base-buffer-name) 'regex))

(defun execrun-set-compilation-index (index lst)
  "Set compilation buffer with INDEX and LST."
  (cond ((< index 0) (setq index (1- (length lst))))
        ((>= index (length lst)) (setq index 0)))
  (switch-to-buffer (nth index lst)))

;;;###autoload
(defun execrun-popup ()
  "Show output window."
  (interactive)
  (let ((output-lst (execrun-list-compilation)))
    (if (= 0 (length output-lst))
        (user-error "[INFO] No output compilation exists")
      (execrun-set-compilation-index 0 output-lst))))

;;
;; (@* "Build & Run" )
;;

(defconst execrun-base-buffer-name "execrun"
  "Base filename for compilation buffer.")

(defun execrun--form-file-prefix ()
  "Form the prefix of the compilation buffer name."
  (format "*%s*: " execrun-base-buffer-name))

;;;###autoload
(defun execrun-switch-to-buffer ()
  "Switch to one of the output buffer."
  (interactive)
  (let* ((output-prefix (execrun--form-file-prefix))
         (output-buf-lst (execrun--get-buffers output-prefix 'string))
         choice)
    (if (not output-buf-lst)
        (user-error "[INFO] No output buffer available: %s" output-buf-lst)
      (setq choice (completing-read "Output buffer: " output-buf-lst))
      (switch-to-buffer choice))))

;;;###autoload
(defun execrun-project-file (file title)
  "Compile FILE from the project with TITLE."
  (interactive)
  (execrun-compile (jcs-find-file-in-project-and-current-dir file title)))

;;;###autoload
(defun execrun-compile (in-op)
  "Compile command rewrapper.
IN-OP : inpuit operation script."
  (let* (;; NOTE: First we need to get the script directory. In order
         ;; to change execute/workspace directory to the current target script's
         ;; directory path.
         (script-dir (f-dirname in-op))
         ;; NOTE: Change the current execute/workspace directory
         ;; to the script directory temporary. So the script will execute
         ;; within the current directory the script is currently in.
         ;;
         ;; Without these lines of code, the script will execute in the
         ;; `default-directory' variables. The `default-directory' variables
         ;; will be the directory path where you start the Emacs. For instance,
         ;; if you start Emacs at path `/usr/home', then the default directory
         ;; will be at `usr/home' directory.
         ;;
         ;; Adding these lines of code if your scirpt is at `/usr/home/project/some-script.sh',
         ;; Then your `default-directory' became `usr/home/project'. Hurray!
         (default-directory script-dir))
    ;; Compile/Execute the target script.
    (compile in-op t)
    (with-current-buffer "*compilation*"
      (rename-buffer (format "%s%s" (execrun--form-file-prefix) (f-filename in-op)) t)
      (execrun-mode 1))
    (message "Executing script file: '%s'" in-op)))

;;
;; (@* "Functions" )
;;

;;;###autoload
(defun execrun-build ()
  "Make the current build."
  (interactive)
  (execrun-project-file execrun-build-script "Build script: "))

;;;###autoload
(defun execrun-run ()
  "Run the current build program."
  (interactive)
  (execrun-project-file execrun-run-script "Run script: "))

;;;###autoload
(defun execrun-previous ()
  "Select the previous compilation buffer."
  (interactive)
  (let ((output-lst (execrun-list-compilation)) (index 0) break)
    (while (and (< index (length output-lst)) (not break))
      (when (equal (current-buffer) (nth index output-lst))
        (bury-buffer)
        (execrun-set-compilation-index (1- index) output-lst)
        (setq break t))
      (cl-incf index))))

;;;###autoload
(defun execrun-next ()
  "Select the next compilation buffer."
  (interactive)
  (let ((output-lst (execrun-list-compilation)) (index 0) break)
    (while (and (< index (length output-lst)) (not break))
      (when (equal (current-buffer) (nth index output-lst))
        (execrun-set-compilation-index (1+ index) output-lst)
        (setq break t))
      (cl-incf index))))

;;;###autoload
(defun execrun-maybe-kill-buffer ()
  "Maybe kill buffer action in `output' buffer."
  (interactive)
  (let ((output-len (length (execrun-list-compilation))) prev-output-buf)
    (when (< 1 output-len)
      (save-window-excursion
        (execrun-previous)
        (setq prev-output-buf (current-buffer))))
    (funcall execrun-kill-buffer-function)
    (when prev-output-buf (switch-to-buffer prev-output-buf))))

(provide 'execrun)
;;; execrun.el ends here
