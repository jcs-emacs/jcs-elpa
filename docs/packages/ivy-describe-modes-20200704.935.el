;;; ivy-describe-modes.el --- Ivy interface to `describe-mode`  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-07-02 19:56:29

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Ivy interface to `describe-mode`.
;; Keyword:
;; Version: 0.2.0
;; Package-Version: 20200704.935
;; Package-Commit: d14986029763858bd72f1fcdc6e7cb70781be6c5
;; Package-Requires: ((emacs "24.4") (ivy "0.13.0"))
;; URL: https://github.com/jcs-elpa/ivy-describe-modes

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
;; Ivy interface to `describe-mode`.
;;

;;; Code:

(require 'ivy)

(defgroup ivy-describe-modes nil
  "Ivy interface to `describe-mode`."
  :prefix "ivy-describe-modes-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/ivy-describe-modes"))

(defconst ivy-describe-modes--prompt "[Describe Modes]: "
  "Prompt string when using `ivy-describe-modes'.")

(defun ivy-describe-modes--is-contain-list-string (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST."
  (cl-some (lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

(defun ivy-describe-modes--minor-mode-list ()
  "Get a list of minor modes."
  (let ($list)
    (mapc (lambda ($mode)
            (condition-case nil
                (when (symbolp $mode)
                  (setq $list (cons (format "%s" $mode) $list)))
              (error nil)))
          minor-mode-list)
    (sort $list 'string<)))

(defun ivy-describe-modes--predicate (ob)
  "Predciate to filter OB to major/minor mode list."
  (and (functionp ob)
       (string-suffix-p "-mode" (format "%s" ob))))

(defun ivy-describe-modes--do-action (cand)
  "Do action after ivy done with CAND."
  (describe-function (intern cand)))

;;;###autoload
(defun ivy-describe-modes-minor ()
  "Describe modes with only minor modes."
  (interactive)
  (ivy-read ivy-describe-modes--prompt
            (ivy-describe-modes--minor-mode-list)
            :require-match t
            :sort t
            :predicate #'ivy-describe-modes--predicate
            :action #'ivy-describe-modes--do-action))

;;;###autoload
(defun ivy-describe-modes-major ()
  "Describe modes with only major modes."
  (interactive)
  (let ((min-lst (ivy-describe-modes--minor-mode-list)))
    (ivy-read ivy-describe-modes--prompt
              obarray
              :require-match t
              :sort t
              :predicate
              (lambda (ob)
                (and (ivy-describe-modes--predicate ob)
                     (not (ivy-describe-modes--is-contain-list-string min-lst (format "%s" ob)))))
              :action #'ivy-describe-modes--do-action)))

;;;###autoload
(defun ivy-describe-modes ()
  "A convenient Ivy version of `describe-mode'."
  (interactive)
  (ivy-read ivy-describe-modes--prompt
            obarray
            :require-match t
            :sort t
            :predicate #'ivy-describe-modes--predicate
            :action #'ivy-describe-modes--do-action))

(provide 'ivy-describe-modes)
;;; ivy-describe-modes.el ends here
