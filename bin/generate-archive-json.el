;;; generate-archive-json.el --- Build archive json  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./bin/prepare.el")

(let (json)
  (dolist (pkg archive-contents)
    (let* ((pkg-name (car pkg)) (desc (cdr pkg))
           (version (aref desc 0))
           (version (mapconcat (lambda (item) (format "%s" item)) version "."))
           (summary (aref desc 2))
           (extras (aref desc 4))
           (url (cdr (assq :url extras)))
           (source (cond ((string-match-p "github" url) "github")
                         ((string-match-p "gitlab" url) "gitlab")
                         (t "git")))
           object)
      (push (cons "name" pkg-name) object)
      (push (cons "summary" summary) object)
      (push (cons "version" version) object)
      (push (cons "url" url) object)
      (push (cons "source" source) object)
      (setq object (reverse object))
      (message "Generating... %s" object)
      (push object json)))
  (write-region (json-encode (reverse json)) nil "./docs/archive.json"))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; generate-archive-json.el ends here
