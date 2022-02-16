;;; generate-archive-json.el --- Build archive json  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./bin/prepare.el")

(defun tree-url (source url commit)
  "Return tree url."
  (pcase source
    ((or "github" "gitlab") (concat url "/tree/" commit))
    (_ url)))

(defun recipe-data (name)
  "Return recipe data."
  (when-let* ((file (format "./recipes/%s" name))
              (content (file-to-string file))
              (data (eval (thing-at-point--read-from-whole-string (concat "'" content)))))
    (pop data)
    data))

(defun recipe-get (name prop)
  "Return recipe data."
  (when-let ((data (recipe-data name))) (plist-get data prop)))

(defun get-fetcher (name) "Get fetcher." (recipe-get name :fetcher))
(defun get-url (name) "Get url." (recipe-get name :url))
(defun get-repo (name) "Get repo." (recipe-get name :repo))

(defun construct-url (name source)
  "Construct the url by SOURCE."
  (when-let ((repo (get-repo name)))
    (pcase (format "%s" source)
      ("github" (concat "https://github.com/" repo))
      ("gitlab" (concat "https://gitlab.com/" repo)))))

(let (json)
  (dolist (pkg archive-contents)
    (let* ((pkg-name (car pkg)) (desc (cdr pkg))
           (version (package-version-join (aref desc 0)))
           (summary (aref desc 2))
           (extras (aref desc 4))
           (source (get-fetcher pkg-name))  ; fetcher
           (url (or (cdr (assq :url extras))         ; get from `archive-contents'
                    (get-url pkg-name)               ; get from `recipe'
                    (construct-url pkg-name source)  ; try construct url
                    ""))
           (commit (cdr (assq :commit extras)))
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
