;;; sideline-eldoc.el --- sideline backend for eldoc.  -*- lexical-binding: t; -*-
;; Package-Version: 20230302.817
;; Package-Commit: 0278dc1f1176dcbb50b755ffa430950dab64b133

;; Copyright (C) 2023  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
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

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `sideline-eldoc-documentation-max-line'
;;    Sideline eldoc show the max line of documentation.
;;    default = 5
;;  `sideline-eldoc-documentation'
;;    If Sideline eldoc show the function documentation.
;;    default = t
;;  `sideline-eldoc-hide-minibuffer'
;;    If hide eldoc message in minibuffer.
;;    default = t

;;; Installation:
;; Manual:
;; Download the source code and put it wherever you like, e.g. into
;; ~/.emacs.d/sideline-eldoc/
;; ```
;; git clone git@github.com:ginqi7/sideline-eldoc.git
;; ```
;; Add the downloaded directory to the load path:
;; ```
;; (add-to-list 'load-path "~/.emacs.d/sideline-eldoc/")
;; (require 'sideline-eldoc)
;; ```
;;

;;; Code:
(require 'sideline)

(defvar sideline-eldoc--message "")

(defcustom sideline-eldoc-documentation-max-line 5
  "Sideline eldoc show the max line of documentation.")

(defcustom sideline-eldoc-documentation t
  "If Sideline eldoc show the function documentation.")

(defcustom sideline-eldoc-hide-minibuffer t
  "If hide eldoc message in minibuffer.")

(defun sideline-eldoc-backend (command)
  "Sideline backend for eldoc.
COMMAND is input parameter."
  (cl-case command
    (`candidates
     (cons :async #'sideline-eldoc--display))))

(defun sideline-eldoc--display (callback &rest _)
  "Execute CALLBACK to display with sideline."
  (when eldoc-mode
    (when-let ((msg sideline-eldoc--message)
               )
      (funcall callback (sideline-eldoc--combine-all-infomations)))))

(defun sideline-eldoc--combine-all-infomations ()
  "Sideline-eldoc combine all information."
  (let ((all-informations
         (append
          (list sideline-eldoc--message)
          (sideline-eldoc--fetch-function-documentation sideline-eldoc--message)
          (sideline-eldoc--parse-function-demo sideline-eldoc--message))))
    (if (eq (sideline-eldoc--order) 'up)
        (reverse all-informations)
      all-informations)))

(defun sideline-eldoc--order ()
  "Get sideline eldoc order."
  (if (member 'sideline-eldoc-backend sideline-backends-right)
      sideline-order-right
    (when (member 'sideline-eldoc-backend sideline-backends-left)
      sideline-order-left)))


(defun sideline-eldoc--parse-function-demo (msg)
  "Eldoc sideline parse function demo by MSG."
  ;; TODO
  nil)

(defun sideline-eldoc--message-function-p (msg)
  "Check if current eldoc message MSG is a function message."
  (string-match ".*: (.*)" msg))

(defun sideline-eldoc--extract-function-symbol (msg)
  "Extract function symbol in MSG."
  (intern (car (string-split msg ":" t " +"))))

(defun sideline-eldoc--parse-function-documentation (documentation)
  "Parse function DOCUMENTATION."
  (let ((doc-lines (string-split documentation "\n" t " +")))
    (if (>= (length doc-lines) sideline-eldoc-documentation-max-line)
        (seq-subseq doc-lines 0 sideline-eldoc-documentation-max-line))))


(defun sideline-eldoc--fetch-function-documentation (msg)
  "Eldoc sideline parse function documentation by MSG."
  (let ((msg-no-properties (substring-no-properties msg)))
    (when (and sideline-eldoc-documentation
               (sideline-eldoc--message-function-p msg-no-properties))
      (sideline-eldoc--parse-function-documentation
       (documentation
        (sideline-eldoc--extract-function-symbol msg-no-properties))))))

(defun sideline-eldoc--extract-message (str &rest args)
  "Extract eldoc message format STR with ARGS."
  (setq sideline-eldoc--message (apply #'format str args)))

(if sideline-eldoc-hide-minibuffer
    (setq eldoc-message-function #'sideline-eldoc--extract-message)
  (advice-add (symbol-value 'eldoc-message-function)
              :before #'sideline-eldoc--extract-message))

(provide 'sideline-eldoc)
;;; sideline-eldoc.el ends here
