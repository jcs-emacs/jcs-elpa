;;; flx-style.el --- Completion style for flx  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-01-19 17:00:12

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Completion style for flx
;; Keyword: flx completion style
;; Version: 0.1.0
;; Package-Version: 20220119.1001
;; Package-Commit: 668968468caa137a0935894d598ce69ad7b3e177
;; Package-Requires: ((emacs "24.3") (flx "0.5"))
;; URL: https://github.com/jcs-elpa/flx-style

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
;; Completion style for flx
;;

;;; Code:

(require 'cl-lib)
(require 'flx)

(defvar flx-style-cache nil
  "Stores flx-cache.")

(defun flx-style-commonality (strs)
  "Return the largest string that fuzzy matches all STRS."
  (cl-letf* ((commonality-cache (make-hash-table :test 'equal :size 200))
             ((symbol-function #'fuzzy-commonality)
              (lambda (strs)
                (let ((hash-value (gethash strs commonality-cache nil)))
                  (if hash-value
                      (if (eq hash-value 'nothing)
                          nil
                        hash-value)

                    (setq strs (mapcar #'string-to-list strs))
                    (let ((res) (tried) (idx))
                      (dolist (char (car strs))
                        (unless (memq char tried)
                          (catch 'notfound
                            (setq idx (mapcar (lambda (str)
                                                (or
                                                 (cl-position char str)
                                                 (throw 'notfound nil)))
                                              strs))
                            (push (cons char
                                        (fuzzy-commonality
                                         (cl-mapcar (lambda (str idx)
                                                      (cl-subseq str (1+ idx)))
                                                    strs idx)))
                                  res)
                            (push char tried))))
                      (setq res (if res
                                    (cl-reduce
                                     (lambda (a b)
                                       (if (> (length a) (length b)) a b))
                                     res)
                                  nil))
                      (puthash strs
                               (if res res 'nothing)
                               commonality-cache)
                      res))))))
    (concat (fuzzy-commonality strs))))

(defun flx-style-find-holes (merged str)
  "Find positions in MERGED, where insertion by the user is likely, wrt. STR"
  (let ((holes) (matches (cdr (flx-score str merged flx-style-cache))))
    (dolist (i (number-sequence 0 (- (length matches) 2)))
      (when (>
             (elt matches (1+ i))
             (1+ (elt matches i)))
        (push (1+ i) holes)))
    (unless (<= (length str) (car (last matches)))
      (push (length merged) holes))
    holes))

(defun flx-style-merge (strs)
  "Merge a collection of strings, including their collective holes"
  (let ((common (flx-style-commonality strs))
        (holes))
    (setq holes (make-vector (1+ (length common)) 0))
    (dolist (str strs)
      (dolist (hole (flx-style-find-holes common str))
        (cl-incf (elt holes hole))))
    (cons common (append holes nil))))

(defun flx-style-completion (string table predicate point &optional all-p)
  "Helper function implementing a fuzzy completion-style"
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (boundaries (completion-boundaries beforepoint table predicate afterpoint))
         (prefix (substring beforepoint 0 (car boundaries)))
         (infix (concat
                 (substring beforepoint (car boundaries))
                 (substring afterpoint 0 (cdr boundaries))))
         (suffix (substring afterpoint (cdr boundaries)))
         ;; |-              string                  -|
         ;;              point^
         ;;            |-  boundaries -|
         ;; |- prefix -|-    infix    -|-  suffix   -|
         ;;
         ;; Infix is the part supposed to be completed by table, AFAIKT.
         (regexp (concat "\\`"
                         (mapconcat
                          (lambda (x)
                            (setq x (string x))
                            (concat "[^" x "]*" (regexp-quote x)))
                          infix
                          "")))
         (completion-regexp-list (cons regexp completion-regexp-list))
         (candidates (or (all-completions prefix table predicate)
                         (all-completions infix table predicate))))

    (if all-p
        ;; Implement completion-all-completions interface
        (when candidates
          ;; Not doing this may result in an error.
          (setcdr (last candidates) (length prefix))
          candidates)
      ;; Implement completion-try-completions interface
      (if (= (length candidates) 1)
          (if (equal infix (car candidates))
              t
            ;; Avoid quirk of double / for filename completion. I don't
            ;; know how this is *supposed* to be handled.
            (when (and (> (length (car candidates)) 0)
                       (> (length suffix) 0)
                       (char-equal (aref (car candidates)
                                         (1- (length (car candidates))))
                                   (aref suffix 0)))
              (setq suffix (substring suffix 1)))
            (cons (concat prefix (car candidates) suffix)
                  (length (concat prefix (car candidates)))))
        (if (= (length infix) 0)
            (cons string point)
          (cl-destructuring-bind (merged . holes)
              (flx-style-merge candidates)
            (cons
             (concat prefix merged suffix)
             (+ (length prefix)
                (cl-position (apply #'max holes) holes)))))))))

;;;###autoload
(defun flx-style-try-completion (string table predicate point)
  "Fuzzy version of completion-try-completion"
  (flx-style-completion string table predicate point))

;;;###autoload
(defun flx-style-all-completions (string table predicate point)
  "Fuzzy version of completion-all-completions"
  (flx-style-completion string table predicate point 'all))

;;;###autoload
(add-to-list 'completion-styles-alist
             '(flx
               flx-style-try-completion
               flx-style-all-completions
               "An intelligent fuzzy matching completion style."))

(provide 'flx-style)
;;; flx-style.el ends here
