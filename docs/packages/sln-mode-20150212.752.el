;;; sln-mode.el --- a major-mode for msvc's *.sln files
;;
;; Copyright 2013 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; Created: 2013
;; Keywords: languages
;; Package-Version: 20150212.752
;; Package-Commit: 0f91d1b957c7d2a7bab9278ec57b54d57f1dbd9c
;; 
;; This file is not part of GNU Emacs.
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;; 
;;; Commentary:
;; 
;; A major-mode for msvc's *.sln files.

(require 'font-lock-ext) ; https://github.com/sensorflo/font-lock-ext/


;;; Variables:
(defvar sln-mode-hook nil
  "Normal hook run when entering sln mode.")

(defconst sln-re-uuid-raw
  (let ((hd "[a-fA-F0-9]"));hexdigit
    (concat hd"\\{8\\}-"hd"\\{4\\}-"hd"\\{4\\}-"hd"\\{4\\}-"hd"\\{12\\}"))
  "Regexp matching an uuid exclusive braces.")

(defconst sln-re-uuid
  (concat "{" sln-re-uuid-raw "}")
  "Regexp matching an uuid inclusive braces.")

(defconst sln-re-project-def
  (concat
   "^\\(?:Project(\"" sln-re-uuid "\")\\s-*=\\s-*" ; type
   "\\(\"\\([^\"\n]*?\\)\"\\)\\s-*,\\s-*"          ; name
   "\"[^\"\n]*?\"\\s-*,\\s-*"                      ; path
   "\\(\"{\\(" sln-re-uuid-raw "\\)}\"\\)\\)")     ; uuid
  "Regexp matching a project definition header line.
Subgroups:
1 project name inclusive quotes
2 project name only
3 project uuid inclusive quotes and braces
4 project uuid only")

(defconst sln-uuid-projecttype-alist
  (list
   '("FAE04EC0-301F-11D3-BF4B-00C04F79EFBC" . "Windows (C#)")
   '("F184B08F-C81C-45F6-A57F-5ABD9991F28F" . "Windows (VB.NET)")
   '("8BC9CEB8-8B4A-11D0-8D11-00A0C91BC942" . "Windows (Visual C++)")
   '("F2A71F9B-5D33-465A-A702-920D77279786" . "Windows (F#)")
   '("349C5851-65DF-11DA-9384-00065B846F21" . "Web Application")
   '("E24C65DC-7377-472B-9ABA-BC803B73C61A" . "Web Site")
   '("F135691A-BF7E-435D-8960-F99683D2D49C" . "Distributed System")
   '("3D9AD99F-2412-4246-B90B-4EAA41C64699" . "Windows Communication Foundation (WCF)")
   '("60DC8134-EBA5-43B8-BCC9-BB4BC16C2548" . "Windows Presentation Foundation (WPF)")
   '("C252FEB5-A946-4202-B1D4-9916A0590387" . "Visual Database Tools")
   '("A9ACE9BB-CECE-4E62-9AA4-C7E7C5BD2124" . "Database")
   '("4F174C21-8C12-11D0-8340-0000F80270F8" . "Database (other project types)")
   '("00D1A9C2-B5F0-4AF3-8072-F6C62B433612" . "SQL Database")
   '("3AC096D0-A1C2-E12C-1390-A8335801FDAB" . "Test")
   '("20D4826A-C6FA-45DB-90F4-C717570B9F32" . "Legacy (2003) Smart Device (C#)")
   '("CB4CE8C6-1BDB-4DC7-A4D3-65A1999772F8" . "Legacy (2003) Smart Device (VB.NET)")
   '("4D628B5B-2FBC-4AA6-8C16-197242AEB884" . "Smart Device (C#)")
   '("68B1623D-7FB9-47D8-8664-7ECEA3297D4F" . "Smart Device (VB.NET)")
   '("14822709-B5A1-4724-98CA-57A101D1B079" . "Workflow (C#)")
   '("D59BE175-2ED0-4C54-BE3D-CDAA9F3214C8" . "Workflow (VB.NET)")
   '("06A35CCD-C46D-44D5-987B-CF40FF872267" . "Deployment Merge Module")
   '("3EA9E505-35AC-4774-B492-AD1749C4943A" . "Deployment Cab")
   '("978C614F-708E-4E1A-B201-565925725DBA" . "Deployment Setup")
   '("AB322303-2255-48EF-A496-5904EB18DA55" . "Deployment Smart Device Cab")
   '("A860303F-1F3F-4691-B57E-529FC101A107" . "Visual Studio Tools for Applications (VSTA)")
   '("BAA0C2D2-18E2-41B9-852F-F413020CAA33" . "Visual Studio Tools for Office (VSTO)")
   '("F8810EC1-6754-47FC-A15F-DFABD2E3FA90" . "SharePoint Workflow")
   '("6D335F3A-9D43-41b4-9D22-F6F17C4BE596" . "XNA (Windows)")
   '("2DF5C3F4-5A5F-47a9-8E94-23B4456F55E2" . "XNA (XBox)")
   '("D399B71A-8929-442a-A9AC-8BEC78BB2433" . "XNA (Zune)")
   '("EC05E597-79D4-47f3-ADA0-324C4F7C7484" . "SharePoint (VB.NET)")
   '("593B0543-81F6-4436-BA1E-4747859CAAE2" . "SharePoint (C#)")
   '("A1591282-1198-4647-A2B1-27E5FF5F6F3B" . "Silverlight")
   '("603C0E0B-DB56-11DC-BE95-000D561079B0" . "ASP.NET MVC 1.0")
   '("F85E285D-A4E0-4152-9332-AB1D724D3325" . "ASP.NET MVC 2.0")
   '("E53F8FEA-EAE0-44A6-8774-FFD645390401" . "ASP.NET MVC 3.0")
   '("E3E379DF-F4C6-4180-9B81-6769533ABE47" . "ASP.NET MVC 4.0")
   '("82B43B9B-A64C-4715-B499-D71E9CA2BD60" . "Extensibility")
   '("2150E333-8FDC-42A3-9474-1A3956D46DE8" . "Solution Folder"))
  "Alist of known projectype uuids.
Source: http://www.mztools.com/articles/2008/mz2008017.aspx at bottom.")

(defvar sln-uuid-hashtable (make-hash-table :test 'equal)
  "Hash table: key=uuid, value=description.")
(make-variable-buffer-local 'sln-uuid-hashtable)

(defvar sln-uuid-reverse-hashtable nil
  "Reverse of `sln-uuid-hashtable', thus key=description value=uuid.")
(make-variable-buffer-local 'sln-uuid-reverse-hashtable)

(defconst sln-font-lock-keywords
  (list
   (list 'sln-keyword-function-put-overlay)
   (list (concat "^\\s-*" sln-re-uuid "\\s-*\\(=\\s-*" sln-re-uuid "\\s-*\\(?:\n\\|\\'\\)\\)")
         (list 1 'font-lock-semi-unimportant t))
   (list sln-re-uuid-raw (list 0 'font-lock-unimportant t))
   ;; _after_ highlighting uuid's as unimportant so the project's uuid is
   ;; highlighted as defined by the following font lock keyword
   (list sln-re-project-def
         (list 1 'font-lock-function-name-face t)
         (list 3 'font-lock-semi-unimportant t))
   ))


;;; Code:
(defun sln-keyword-function-put-overlay(end)
  "Puts an before-string overlay on next uuid containing its description.
Intended to be used as a keyword function for font-lock. Does
nothing when the uuid is a definition rather than a reference."
  (when (re-search-forward (concat "{\\(" sln-re-uuid-raw "\\)\\(}\\)") end t)
    (let* ((o (make-overlay (match-beginning 2) (match-end 2)))
           (projectname
            (gethash (match-string-no-properties 1) sln-uuid-hashtable "unknown")))
      (unless (save-match-data
                (and (save-excursion
                       (beginning-of-line)
                       (looking-at sln-re-project-def))
                     (< (match-beginning 1) (point))))
        (overlay-put o 'before-string (concat "(=" projectname ")")))
      t)))

(defun sln-parse()
  "Parses current buffer to generate `sln-uuid-hashtable'"
  (interactive)
  (save-excursion
    (save-restriction
      (clrhash sln-uuid-hashtable)
      (mapc (lambda(x) (puthash (car x) (cdr x) sln-uuid-hashtable))
            sln-uuid-projecttype-alist)
      (goto-char (point-min))
      (while (re-search-forward sln-re-project-def nil t)
        (puthash (match-string-no-properties 4) (match-string-no-properties 2) sln-uuid-hashtable))

      (setq sln-uuid-reverse-hashtable
            (make-hash-table
             :test (hash-table-test sln-uuid-hashtable)
             :size (hash-table-size sln-uuid-hashtable)))
      (maphash (lambda(k v) (puthash v k sln-uuid-reverse-hashtable))
               sln-uuid-hashtable))))

(defun sln-replace-description-by-uuid()
  "Replaces the description at point with it's associated uuid.

The uuid associated with the given description is looked up in
`sln-uuid-reverse-hashtable'.

The description is either enclosed in curly braces or is a single
word, in that order of priority. Point can be either within the
description, at the end of it, or at the beginning, in that order
of priority.

If the 'description' is already an uuid occuring in the table,
then nothing is done."
  (interactive)
  (if (or
       ;; description in curly braces
       (and (save-excursion
              (re-search-backward "{[^{}\"\n]*}?\\s-*\\=" nil t)
              (looking-at "{\\([^{}\"\n]*\\)}")))
       ;; description as a single word
       (and (save-excursion
              (re-search-backward "\\b\\sw+\\s-*\\=" nil t)
              (looking-at "\\(\\sw+\\)"))))
      (let* ((description-raw (match-string-no-properties 1))
             (description-full (match-string-no-properties 0))
             (uuid (gethash description-raw sln-uuid-reverse-hashtable))
             (replace-match-with
              (lambda (replacement)
                (goto-char (match-beginning 1))
                (delete-region (match-beginning 0) (match-end 0))
                (insert "{" replacement "}"))))
        (if uuid
            (funcall replace-match-with uuid)
          (cond
           ((save-match-data (string-match (concat "\\`" sln-re-uuid "\\'") description-full))
            (message "'%s' is already an valid uuid" description-raw))
           ((save-match-data (string-match (concat "\\`" sln-re-uuid-raw "\\'") description-full))
            (funcall replace-match-with description-raw)
            (message "'%s' is already an valid uuid. Canonicalized it by enclosing it in curly braces {}."
                     description-raw))
           (t
            (error "Don't know uuid of '%s'" description-raw)))))
    (error "Point is not within or next to an description")))

(defun sln-replace-description-by-uuid-dwim()
  "Do-what-I-mean variant of `sln-replace-description-by-uuid'."
  (interactive)
  (call-interactively 'sln-replace-description-by-uuid)
  ;; auto complete redundant part of an ProjectDependencies list element
  (save-excursion
    (end-of-line)
    (skip-syntax-backward "-")
    (when (looking-back
           (concat
            "^\\s-*ProjectSection(ProjectDependencies)\\s-*=.*\n"
            "\\(?:\\s-*" sln-re-uuid "\\s-*=\\s-*" sln-re-uuid "\\s-*\n\\)*"
            "\\s-*\\(" sln-re-uuid "\\)\\s-*=?"))
      (if (eq (char-before) ?\=)
          (insert " ")
        (insert " = "))
      (insert (match-string-no-properties 1))
      (delete-region (point) (re-search-forward "\\s-*?$" nil t))
      (indent-according-to-mode))))

(defun sln-unfontify-region-function (beg end)
  "sln-mode's function for `font-lock-unfontify-region-function'."
  (font-lock-default-unfontify-region beg end)
  
  ;; todo: this is an extremely brute force solution and interacts very badly
  ;; with many (minor) modes using overlays such as flyspell or ediff
  (remove-overlays beg end))

(defun sln-indent-line-function ()
  "sln-mode's function for `indent-line-function'."
  ;; In the regexps [ \t] instead \s- is used since only horizontal blanks are
  ;; of interest
  (save-excursion
    (let* ((re-heading-start "\\(?:Project\\|Global\\)\\(?:Section\\)?\\b")
           (re-heading-end (concat "End" re-heading-start))
           (above-ref-column
            (save-excursion
              (beginning-of-line 0)
              (re-search-forward "[ \t]*")
              (when (looking-at re-heading-start)
                (forward-char 2))
              (current-column)))
           (offset 0)
           (final-column 0))
      (beginning-of-line)
      (when (looking-at (concat "[ \t]*" re-heading-end))
        (setq offset -2))
      (setq final-column (+ above-ref-column offset))
      (unless (equal final-column
                     (save-excursion
                       (re-search-forward "\\=[ \t]*")
                       (current-column)))
        (delete-region (point) (re-search-forward "\\=[ \t]*"))
        (indent-to final-column))))
  ;; when within leading horizontal blanks move point accross them
  (when (looking-back "^[ \t]*")
    (re-search-forward "\\=[ \t]+")))

;;;###autoload
(define-derived-mode sln-mode text-mode "sln"
  "Major mode for editing msvc's *.sln files.
Turning on sln mode runs the normal hook `sln-mode-hook'."
  
  ;; syntax table
  (modify-syntax-entry ?$ ".")
  (modify-syntax-entry ?% ".")
  (modify-syntax-entry ?& ".")
  (modify-syntax-entry ?' ".")
  (modify-syntax-entry ?` ".")
  (modify-syntax-entry ?* ".")
  (modify-syntax-entry ?+ ".")
  (modify-syntax-entry ?. ".")
  (modify-syntax-entry ?/ ".")
  (modify-syntax-entry ?< ".")
  (modify-syntax-entry ?= ".")
  (modify-syntax-entry ?> ".")
  (modify-syntax-entry ?\\ ".")
  (modify-syntax-entry ?| ".")
  (modify-syntax-entry ?\; ".")
  (modify-syntax-entry ?\" "\"")
  (modify-syntax-entry ?\_ "w")
  (modify-syntax-entry ?\- "w")
  (modify-syntax-entry ?\# "<")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?\r ">")

  ;; comments
  (set (make-local-variable 'comment-column) 0)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "\\(#[ \t]*\\)")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(?:\n\\|\\'\\)")
  
  ;; font lock
  (set (make-local-variable 'font-lock-defaults)
       '(sln-font-lock-keywords))
  (set (make-local-variable 'font-lock-unfontify-region-function)
       'sln-unfontify-region-function)
  
  ;; indentation
  (set (make-local-variable 'indent-line-function)
       'sln-indent-line-function)
  (set (make-local-variable 'indent-tabs-mode) t)

  ;; (easy) menu
  (easy-menu-define
    sln-mode-menu sln-mode-map "Menu for sln mode"
    `("sln"
      ["Replace description by uuid dwim" sln-replace-description-by-uuid-dwim]))
  ;; easy-menu-add is called later

  ;; auto runned stuff
  (sln-parse)
  (run-hooks 'sln-mode-hook)

  ;; depended on sln-mode-hooks already runned

  ;; so menu can capture bindings potentially defined by hooks
  (easy-menu-add sln-mode-menu))


(provide 'sln-mode)

;;; sln-mode.el ends here
