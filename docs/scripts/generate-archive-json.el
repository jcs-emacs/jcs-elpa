;;; generate-archive-json.el --- Build archive json  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'thingatpt)
(require 'json)

(defconst archive-contents-string
  (with-temp-buffer
    (or (ignore-errors (insert-file-contents "../elpa/archive-contents"))
        (ignore-errors (insert-file-contents "./docs/elpa/archive-contents")))  ; CI
    (buffer-string))
  "Archive content string.")

(defconst archive-contents
  (eval (thing-at-point--read-from-whole-string
         (concat "'" archive-contents-string)))
  "Turn it into lisp object.")

(pop archive-contents)  ; remove 1

(let (json)
  (dolist (pkg archive-contents)
    (let* ((pkg-name (car pkg)) (desc (cdr pkg))
           (version (aref desc 0))
           (summary (aref desc 2))
           (extras (aref desc 4))
           (url (cdr (assq :url extras)))
           object)
      (push (cons "name" pkg-name) object)
      (push (cons "summary" summary) object)
      (push (cons "version" version) object)
      (push (cons "url" url) object)
      (push object json)))
  (write-region (json-encode json) nil "./docs/archive.json")
  (message "json: %s" (with-temp-buffer
                        (insert-file-contents "./docs/archive.json")
                        (buffer-string)))
  )

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; generate-archive-json.el ends here
