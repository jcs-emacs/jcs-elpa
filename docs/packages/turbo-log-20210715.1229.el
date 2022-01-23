;;; turbo-log.el --- Automating the process of writing meaningful log messages  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-02-22 14:27:53

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Automating the process of writing meaningful log messages.
;; Keyword: log debug
;; Version: 0.1.0
;; Package-Version: 20210715.1229
;; Package-Commit: ffce6014f0ceae9e4cab07d520a0326dd5106a02
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/jcs-elpa/turbo-log

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
;; Automating the process of writing meaningful log messages.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup turbo-log nil
  "Automating the process of writing meaningful log messages."
  :prefix "turbo-log-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/turbo-log"))

(defcustom turbo-log-formats
  '((actionscript-mode     . "trace(\"%s\" + %s);")
    (c++-mode              . "std::count << \"%s\" << %s;")
    (csharp-mode           . "Console.WriteLine(\"%s\" + %s);")
    (emacs-lisp-mode       . "(message \"%s%%s\" %s)")
    (go-mode               . "fmt.Println(\"%s\", %s)")
    (groovy-mode           . "println \"%s\" + %s")
    (java-mode             . "System.out.println(\"%s\" + %s);")
    (javascript-mode       . "console.log(\"%s\" + %s);")
    (js-mode               . "console.log(\"%s\" + %s);")
    (js2-mode              . "console.log(\"%s\" + %s);")
    (js3-mode              . "console.log(\"%s\" + %s);")
    (lisp-interaction-mode . "(message \"%s%%s\" %s)")
    (lisp-mode             . "(message \"%s%%s\" %s)")
    (lua-mode              . "io.write(\"%s\" + %s)")
    (objc-mode             . "NSLog(@\"%s%%@\", %s);")
    (php-mode              . "echo \"%s\" . %s;")
    (python-mode           . "print(\"%s\", %s)")
    (rjsx-mode             . "console.log(\"%s\" + %s);")
    (ruby-mode             . "puts \"%s\" + %s")
    (rust-mode             . "println!(\"%s{}\", %s);")
    (scala-mode            . "println(\"%s\" + %s)")
    (swift-mode            . "print(\"%s\", %s)")
    (typescript-mode       . "console.log(\"%s\" + %s);")
    (web-mode              . "console.log(\"%s\" + %s);"))
  "Alist for logging format."
  :type 'list
  :group 'turbo-log)

(defcustom turbo-log-prefix "╘[TL] "
  "Prefix string inserted before variable name."
  :type 'string
  :group 'turbo-log)

(defcustom turbo-log-prefix-delimiter ": "
  "The delimiter between prefix and log."
  :type 'string
  :group 'turbo-log)

(defcustom turbo-log-prefix-intial t
  "If non-nil, have variable name to prefix initial."
  :type '(choice (const :tag "No initial value" nil)
                 (const :tag "Variable name as initial value" t)
                 string)
  :group 'turbo-log)

(defcustom turbo-log-no-ask nil
  "If non-nil, do not ask prefix."
  :type 'boolean
  :group 'turbo-log)

;;
;; (@* "Util" )
;;

(defun turbo-log--inside-comment-or-string-p ()
  "Return non-nil if point is inside comment or string."
  (or (nth 4 (syntax-ppss)) (nth 8 (syntax-ppss))))

(defun turbo-log--char-count (str)
  "Return the character count from region STR."
  (length (split-string str " " t)))

;;
;; (@* "Core" )
;;

(defun turbo-log--get-prefix (var)
  "Return prefix by customization.

Argument VAR is the variable name for initial value."
  (or (and (stringp turbo-log-prefix-intial)
           turbo-log-prefix-intial)
      (if turbo-log-prefix-intial var "")))

(defun turbo-log--insert (var)
  "Insert VAR by format."
  (let ((fmt (cdr (assoc major-mode turbo-log-formats))))
    (if (null fmt)
        (user-error "[WARNING] No turbo log format found")
      (let* ((prefix (if turbo-log-no-ask (turbo-log--get-prefix var)
                       (read-string "Log Prefix: " (turbo-log--get-prefix var))))
             (insertion (concat turbo-log-prefix prefix turbo-log-prefix-delimiter)))
        (goto-char (line-end-position))
        (insert "\n") (indent-for-tab-command)
        (insert (format fmt insertion var))))))

(defun turbo-log-string (str)
  "Log the STR by it's current `major-mode'."
  (deactivate-mark)
  (setq str (string-trim str))
  (cond ((not (= (turbo-log--char-count str) 1))
         (user-error "[WARNING] Multiple tokens region selected"))
        ((turbo-log--inside-comment-or-string-p)
         (user-error "[WARNING] Inside comment or string symbol"))
        (t (turbo-log--insert str))))

;;;###autoload
(defun turbo-log (beg end)
  "Turbo log the current selected region.

Arguments BEG and END are region parameters."
  (interactive "r")
  (if (not (use-region-p))
      (user-error "[WARNING] No region selected")
    (turbo-log-string (buffer-substring beg end))))

(provide 'turbo-log)
;;; turbo-log.el ends here
