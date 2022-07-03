;;; company-cmd.el --- company backend for cmd/batch -*- lexical-binding: t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart
;; Maintainer: Jen-Chieh Shen <jcs090218@gmail.com>
;; URL: https://github.com/elp-revive/company-cmd
;; Package-Version: 20220703.1546
;; Package-Commit: 367d93a32990b07baf3fe97a64a510407ffc14c8
;; Package-Requires: ((emacs "26.1") (company "0.8.12") (ht "2.4"))
;; Keywords: convenience

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple company-mode completion backend for windows batch/dos scripting modes.
;; Offers completion for 80-90 basic commands returned by 'help'.

;; Usage:
;;
;; Add to a suitable location on the `load-path' and either require or autoload
;; `company-cmd'.
;;
;; See: [cmd-mode](http://github.com/nverno/cmd-mode) for combining with
;; completion-at-point (not provided in base `bat-mode').

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'company)
(require 'ht)

(defgroup company-cmd nil
  "CMD/DOS completion backend."
  :group 'company
  :prefix "company-cmd-")

(defcustom company-cmd-modes '(bat-mode dos-mode cmd-mode ntcmd-mode)
  "Various modes for editing windows batch files."
  :type '(repeat function))

(defvar company-cmd--candidates ()
  "List of completion candidates and meta info.")

(defvar company-cmd--doc-buffers (ht-create)
  "Store doc-buffer cache.")

(defun company-cmd--candidates ()
  "Build/return list of commands to offer for completion."
  (or company-cmd--candidates
      (let ((raw
             (split-string
              (replace-regexp-in-string
               "\n\\s-+" " "
               (shell-command-to-string
                "help | findstr /b /v /c:\"For more\""))
              "\n" t)))
        (setq company-cmd--candidates
              (mapcar #'(lambda (str)
                          (let ((pos (string-match "\\s-" str)))
                            (cons (substring str 0 pos)
                                  (replace-regexp-in-string
                                   "^\\s-+" " "
                                   (substring str (1+ pos) (length str))))))
                      raw)))))

(defun company-cmd--prefix ()
  "Prefix for bat completion."
  (and (memq major-mode company-cmd-modes)
       (not (company-in-string-or-comment))
       (company-grab-symbol)))

(defun company-cmd--meta (candidate)
  "Return short documentation string for CANDIDATE."
  (cdr (assoc-string candidate company-cmd--candidates t)))

(defun company-cmd--doc (candidate)
  "Return buffer with detailed help for CANDIDATE."
  (company-doc-buffer
   (or (ht-get company-cmd--doc-buffers candidate)
       (let ((doc (with-temp-buffer
                    (call-process "cmd.exe" nil t nil "/c" (concat " help " candidate))
                    (buffer-string))))
         (ht-set company-cmd--doc-buffers candidate doc)
         doc))))

(defun company-cmd-offer-candidates (arg)
  "Offer completion candidates, if an uppercase character is found,
the candidates are all uppercase."
  (let* ((case-fold-search nil)
         (to-upper (string-match-p "[A-Z]+" arg))
         (completion-ignore-case t)
         (res (all-completions arg (company-cmd--candidates))))
    (if to-upper res
      (mapcar #'downcase res))))

;;;###autoload
(defun company-cmd (command &optional arg &rest ignored)
  "Windows batch/dos script backend for company-mode."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-cmd))
    (prefix (company-cmd--prefix))
    (meta (company-cmd--meta arg))
    (candidates (company-cmd-offer-candidates arg))
    (doc-buffer (company-cmd--doc arg))))

(provide 'company-cmd)
;;; company-cmd.el ends here
