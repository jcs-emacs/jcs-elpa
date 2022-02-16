;;; generate-archive-json.el --- Build archive json  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./bin/prepare.el")

(defconst supported-source '("github" "gitlab")
  "List of supported sources.")

(defun tree-url (source url commit)
  "Return tree url."
  (if (member source supported-source) (concat url "/tree/" commit) url))

(let (json)
  (dolist (pkg archive-contents)
    (let* ((pkg-name (car pkg)) (desc (cdr pkg))
           (version (package-version-join (aref desc 0)))
           (summary (aref desc 2))
           (extras (aref desc 4))
           (url (cdr (assq :url extras)))
           (commit (cdr (assq :commit extras)))
           (source (or (cl-some (lambda (elm) (and (string-match-p elm url) elm))
                                supported-source)
                       (and (string-match-p "hg[.]" url) "hg")
                       "git"))
           (tree (tree-url source url commit))
           object)
      (setq object
            `(("name"    . ,pkg-name)
              ("summary" . ,summary)
              ("version" . ,version)
              ("url"     . ,url)
              ("source"  . ,source)
              ("commit"  . ,commit)
              ("tree"    . ,tree)))
      (message "Generating... %s" object)
      (push object json)))
  (write-region (json-encode (reverse json)) nil "./docs/archive.json"))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; generate-archive-json.el ends here
