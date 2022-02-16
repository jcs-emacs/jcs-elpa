;;; prepare.el --- Prepration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(require 'thingatpt)
(require 'json)

(defconst archive-contents-string
  (with-temp-buffer
    (or (ignore-errors (insert-file-contents "../docs/packages/archive-contents"))
        (ignore-errors (insert-file-contents "./docs/packages/archive-contents")))  ; CI
    (buffer-string))
  "Archive content string.")

(defconst archive-contents
  (eval (thing-at-point--read-from-whole-string
         (concat "'" archive-contents-string)))
  "Turn it into lisp object.")

(pop archive-contents)  ; remove 1

(message "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
(message archive-contents)  ; log out `archive-contents'
(message "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; prepare.el ends here
