;;; balanced-windows.el --- Keep windows balanced -*- lexical-binding: t; -*-

;; Author: wouter bolsterlee <wouter@bolsterl.ee>
;; Maintiainer: Jen-Chieh Shen <jcs090218@gmail.com>
;; Keywords: convenience
;; Package-Version: 20220919.824
;; Package-Commit: fd360ca6416e56d8df92bbcb0f7aec37e201f2e7
;; URL: https://github.com/elp-revive/emacs-balanced-windows
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0.0

;; Copyright 2019 wouter bolsterlee
;; Copyright 2022 Jen-Chieh Shen

;; Licensed under the 3-Clause BSD License.

;;; Commentary:

;; Automatically keep windows balanced.

;;; Code:

(defvar evil-auto-balance-windows)

(defgroup balanced-windows nil
  "Keep windows balanced."
  :group 'windows
  :prefix "balanced-windows-")

(defcustom balanced-windows-commands
  '(delete-window quit-window split-window)
  "Functions needing advice to keep windows balanced."
  :type '(repeat function)
  :group 'balanced-windows)

(defcustom balanced-windows-interactive-only t
  "Balance windows only when called interactively."
  :type 'boolean
  :group 'balanced-windows)

(defun balanced-windows--advice (&rest _ignored)
  "Balance windows (intended as :after advice); any args are ignored."
  (when (or (null balanced-windows-interactive-only)
            (and (called-interactively-p 'interactive)
                 (memq this-command balanced-windows-commands)))
    (balance-windows)))

;;;###autoload
(define-minor-mode balanced-windows-mode
  "Global minor mode to keep windows balanced at all times."
  :global t
  (dolist (fn balanced-windows-commands)
    (if balanced-windows-mode
        (advice-add fn :after #'balanced-windows--advice)
      (advice-remove fn #'balanced-windows--advice)))
  (when balanced-windows-mode
    (balance-windows))
  (when (featurep 'evil)
    (setq evil-auto-balance-windows balanced-windows-mode)))

(provide 'balanced-windows)
;;; balanced-windows.el ends here
