;;; generate-archive-json.el --- Build archive json  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'thingatpt)

(defconst archive-contents-string
  (with-temp-buffer
    (or (ignore-errors (insert-file-contents "../elpa/archive-contents"))
        (ignore-errors (insert-file-contents "./elpa/archive-contents")))
    (buffer-string))
  "Archive content string.")

(defconst archive-contents
  (eval (thing-at-point--read-from-whole-string
         (concat "'" archive-contents-string)))
  "Turn it into lisp object..")

(pop archive-contents)  ; remove 1

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; generate-archive-json.el ends here
