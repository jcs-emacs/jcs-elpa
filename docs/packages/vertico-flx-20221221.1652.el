;;; vertico-flx.el --- Flx integration for vertico  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-04-19 17:15:39

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/vertico-flx
;; Package-Version: 20221221.1652
;; Package-Commit: 3220d3bd358a720ba92086a89c2e86075d46c7db
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (vertico "0.22") (flx "0.5") (flx-style "0.1.1") (ht "2.0") (f "0.20.0") (mbs "0.1.0"))
;; Keywords: convenience vertico flx

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
;; Flx integration for vertico.
;;

;;; Code:

(require 'f)
(require 'ht)
(require 'mbs)

(require 'vertico)
(require 'flx)
(require 'flx-style)

(defgroup vertico-flx nil
  "Flx integration for vertico."
  :prefix "vertico-flx-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/vertico-flx"))

(defvar vertico-flx--old-completion-style nil
  "Different completion style when completing using minbuffer.")

(defvar vertico-flx--sorting nil
  "Return non-nil if currently sorting.")

;;
;; (@* "Util" )
;;

(defmacro vertico-flx--with-minibuffer-env (&rest body)
  "Execute BODY with minibuffer variables."
  (declare (indent 0) (debug t))
  `(let ((prompt (minibuffer-prompt))
         (contents (minibuffer-contents)))
     ,@body))

(defun vertico-flx--directory-p (path)
  "Return non-nil if PATH is a directory path."
  (and (file-exists-p path) (file-directory-p path)))

;;
;; (@* "Fuzzy Sorting" )
;;

(defun vertico-flx--sort-candidates-by-function (candidates prefix fnc &optional flip)
  "Sort CANDIDATES with PREFIX and FNC.

If optional argument FLIP is non-nil, reverse query and pattern order."
  (let ((scoring-table (ht-create)) scoring-keys)
    (dolist (cand candidates)
      (when-let*
          ((scoring (ignore-errors
                      (if flip (funcall fnc prefix cand)
                        (funcall fnc cand prefix))))
           (score (cond ((listp scoring) (nth 0 scoring))
                        ((vectorp scoring) (aref scoring 0))
                        ((numberp scoring) scoring)
                        (t 0))))
        ;; XXX ht causes unknown error on start, use regular hash table functions
        ;; for now
        (unless (gethash score scoring-table) (setf (gethash score scoring-table) nil))
        (push cand (gethash score scoring-table))))
    ;; Get all keys, and turn into a list.
    (ht-map (lambda (score-key _) (push score-key scoring-keys)) scoring-table)
    (setq scoring-keys (sort scoring-keys #'>)  ; Sort keys in order
          candidates nil)  ; Clean up, and ready for final output
    (dolist (key scoring-keys)
      (let ((cands (ht-get scoring-table key)))
        (setq candidates (append candidates cands)))))
  candidates)

;;
;; (@* "Multiform" )
;;

;;;###autoload
(defun vertico-flx-sort-default (all)
  "Sort candidates ALL."
  (vertico-flx--with-minibuffer-env
    (setq vertico-flx--sorting nil)
    (let ((base #'vertico-sort-history-length-alpha))
      ;; Final output
      (if (string-empty-p contents)  ; Empty, return raw
          (if (null base) all
            (cond ((functionp base) (funcall base all))
                  ((listp base) base)))
        (setq vertico-flx--sorting t)
        ;; Return fuzzy order
        (vertico-flx--sort-candidates-by-function all contents #'flx-score)))))

;;;###autoload
(defun vertico-flx-sort-files (all)
  "Sort candidates ALL for files."
  (vertico-flx--with-minibuffer-env
    (setq vertico-flx--sorting nil)
    (let ((input (if (and (string-suffix-p "/" contents)
                          (vertico-flx--directory-p contents))
                     ""
                   (f-filename contents))))
      (if (string-empty-p input)
          (sort (sort all #'string-lessp)
                (lambda (var1 var2)
                  (and (string-suffix-p "/" var1)
                       (not (string-suffix-p "/" var2)))))
        (setq vertico-flx--sorting t)
        (vertico-flx--sort-candidates-by-function all input #'flx-score)))))

;;
;; (@* "Entry" )
;;

(defvar vertico-flx--minibuffer-setup-p nil
  "Flag to make sure `minibuffer-setup' is executed once.")

(defun vertico-flx--minibuffer-setup (&rest _)
  "Hook for minibuffer setup."
  (unless vertico-flx--minibuffer-setup-p
    (setq vertico-flx--old-completion-style completion-styles
          completion-styles '(flx)
          vertico-flx--minibuffer-setup-p t)))

(defun vertico-flx--minibuffer-exit (&rest _)
  "Hook for minibuffer exit."
  (setq completion-styles vertico-flx--old-completion-style
        vertico-flx--minibuffer-setup-p nil))

(defun vertico-flx--enable ()
  "Enable `vertico-flx-mode'."
  (add-hook 'minibuffer-setup-hook #'vertico-flx--minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'vertico-flx--minibuffer-exit))

(defun vertico-flx--disable ()
  "Disable `vertico-flx-mode'."
  (remove-hook 'minibuffer-setup-hook #'vertico-flx--minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'vertico-flx--minibuffer-exit))

;;;###autoload
(define-minor-mode vertico-flx-mode
  "Minor mode `vertico-flx-mode'."
  :global t
  :require 'vertico-flx-mode
  :group 'vertico-flx
  (if vertico-flx-mode (vertico-flx--enable) (vertico-flx--disable)))

(provide 'vertico-flx)
;;; vertico-flx.el ends here
