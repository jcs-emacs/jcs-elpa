;;; company-meta-net.el --- Company completion for C# project using meta-net  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Shen, Jen-Chieh
;; Created date 2021-06-24 21:19:45

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/company-meta-net
;; Package-Version: 20220704.631
;; Package-Commit: 0abcd6148fdea09dc7c65dce4d71a1aa78554873
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (company "0.8.12") (meta-net "1.1.0") (ht "2.3"))
;; Keywords: convenience csproj csharp company

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
;; Company completion for C# project using meta-net
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'company)
(require 'meta-net)
(require 'ht)

(defgroup company-meta-net nil
  "Company completion for C# project using meta-net."
  :prefix "company-meta-net-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/company-meta-net"))

(defcustom company-meta-net-active-modes
  '(csharp-mode csharp-tree-sitter-mode)
  "Major modes that allow completion."
  :type 'list
  :group 'meta-view)

(defcustom company-meta-net-display-annotation t
  "If non-nil, display annotation."
  :type 'boolean
  :group 'eldoc-meta-net)

(defcustom company-meta-net-display-document t
  "If non-nil, display document."
  :type 'boolean
  :group 'eldoc-meta-net)

;; These keywords are grab from `csharp-mode'
(defconst company-meta-net--csharp-keywords
  (append
   '("class" "interface" "struct")
   '("bool" "byte" "sbyte" "char" "decimal" "double" "float" "int" "uint"
     "long" "ulong" "short" "ushort" "void" "object" "string" "var")
   '("typeof" "is" "as")
   '("enum" "new")
   '("using")
   '("abstract" "default" "final" "native" "private" "protected"
     "public" "partial" "internal" "readonly" "static" "event" "transient"
     "volatile" "sealed" "ref" "out" "virtual" "implicit" "explicit"
     "fixed" "override" "params" "async" "await" "extern" "unsafe"
     "get" "set" "this" "const" "delegate")
   '("select" "from" "where" "join" "in" "on" "equals" "into"
     "orderby" "ascending" "descending" "group" "when"
     "let" "by" "namespace")
   '("do" "else" "finally" "try")
   '("for" "if" "switch" "while" "catch" "foreach" "fixed" "checked"
     "unchecked" "using" "lock")
   '("break" "continue" "goto" "throw" "return" "yield")
   '("true" "false" "null" "value")
   '("base" "operator"))
  "Some C# keywords to eliminate namespaces.")

(defvar-local company-meta-net--namespaces nil
  "Where store all the parsed namespaces.")

(defvar-local company-meta-net--candidates (ht-create)
  "Record candidates with it's information.")

(defvar company-meta-net-show-debug nil
  "Show the debug message from this package.")

;;
;; (@* "Util" )
;;

(defun company-meta-net-debug (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when company-meta-net-show-debug (apply 'message fmt args)))

;;
;; (@* "Xmls" )
;;

(defvar-local company-meta-net--xmls nil
  "Cache records a list of assembly xml file path.")

(defun company-meta-net--all-xmls (&optional refresh)
  "Return full list of assembly xml files.

If REFRESH is non-nil, refresh cache once."
  (when (or refresh (null company-meta-net--xmls))
    (setq company-meta-net--xmls (meta-net-csproj-xmls meta-net-csproj-current))
    (cl-delete-duplicates company-meta-net--xmls))
  company-meta-net--xmls)

;;
;; (@* "Core" )
;;

(defun company-meta-net--prepare ()
  "Prepare this package wit meta-net."
  (when (memq major-mode company-meta-net-active-modes)
    (unless meta-net-csproj-current (meta-net-read-project))
    meta-net-csproj-current))

(defun company-meta-net--grab-namespaces ()
  "Parsed namespaces from current buffer."
  (setq company-meta-net--namespaces nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[,.:]*[ \t\n]*\\([a-z-A-Z0-9_-]+\\)[ \t\n]*[.;{]+" nil t)
      (save-excursion
        (forward-symbol -1)
        (unless (company-in-string-or-comment)
          (when-let ((symbol (thing-at-point 'symbol)))
            (push symbol company-meta-net--namespaces))))))
  (setq company-meta-net--namespaces (reverse company-meta-net--namespaces)
        company-meta-net--namespaces (delete-dups company-meta-net--namespaces)
        company-meta-net--namespaces (cl-remove-if (lambda (namespace)
                                                     (member namespace company-meta-net--csharp-keywords))
                                                   company-meta-net--namespaces)))

(defun company-meta-net--match-name (type)
  "Return non-nil, if the TYPE match the current namespace list.

The argument TYPE is a list of namespace in string.  For instance,

   using System.Collections;  => '(System Collections)

We use this to eliminate not possible candidates."
  (let ((match t) (len (length type)) (index 0) item)
    (while (and match (< index len))
      (setq item (nth index type)
            index (1+ index)
            match (member item company-meta-net--namespaces)))
    match))

;;
;; (@* "Company" )
;;

(defun company-meta-net--add-candidates (name annotation doc-buffer)
  "Add a candidate with it's display information.

Argument NAME is a candidate string.  ANNOTATION and DOC-BUFFER are NAME's
display information."
  (ht-set company-meta-net--candidates name
          (list annotation
                doc-buffer)))

(defun company-meta-net--add-tag-data (tag-data annotation)
  "Add TAG-DATA to candidates list.

ANNOTATION is needed before hand."
  (let ((keys (ht-keys tag-data)) summary data)
    (dolist (key keys)
      (unless (string-match-p "(" key)  ; avoid method with full information
        (setq data (ht-get tag-data key)
              summary (ht-get data 'summary))
        (company-meta-net--add-candidates key annotation summary)))))

(defun company-meta-net--prefix ()
  "Return the string represent the prefix."
  (when (and (not (company-in-string-or-comment)) (company-meta-net--prepare))
    (or (company-grab-symbol-cons "\\." 1) 'stop)))

(defun company-meta-net--candidates ()
  "Return a possible candidates from current point."
  (company-meta-net--grab-namespaces)
  (ht-clear company-meta-net--candidates)
  (let* ((xmls (company-meta-net--all-xmls))  ; Get the list of xml files from current project
         (xmls-len (length xmls))      ; length of the xmls
         (xml-index 0)                 ; index search through all `xmls`
         xml          ; current xml path as key
         type         ; xml assembly type
         comp-name    ; name of the type, the last component from the type
         splits       ; temporary list to chop namespace, use to produce `comp-name`
         namespaces)
    (while (< xml-index xmls-len)
      (setq xml (nth xml-index xmls)
            xml-index (1+ xml-index))
      (let* ((types (meta-net-xml-types xml))
             (types-len (length types))
             (type-index 0))
        (while (< type-index types-len)
          (setq type (nth type-index types)
                type-index (1+ type-index)
                splits (split-string type "\\.")
                comp-name (nth (1- (length splits)) splits)
                namespaces (butlast splits))
          ;; Check if all namespaces appears in the buffer,
          ;;
          ;; We use `butlast' to get rid of the component name because we do
          ;; allow the same level candidates.
          ;;
          ;; For example, `NamespaceA` contains `classA` and `classB`, and we
          ;; ignore the check of `classA` and `classB` in order to let them
          ;; both appears in candidates list.
          (when (company-meta-net--match-name namespaces)
            (company-meta-net-debug "\f")
            (company-meta-net-debug "xml: %s" xml)
            (company-meta-net-debug "Type: %s" type)
            (company-meta-net-debug "Name: %s" comp-name)

            (dolist (namespace namespaces)  ; add all namespaces
              (company-meta-net--add-candidates
               namespace "(Namespace)"  ; annotation
               ""))                     ; doc-buffer, namespace has no document

            ;; Add for same level
            (company-meta-net--add-candidates
             comp-name "(Type)"                 ; annotation
             (meta-net-type-summary xml type))  ; doc-buffer

            ;; Variable `company-meta-net--namespaces' contains the type as
            ;; well, so we use this to see if the type appears in the buffer.
            ;;
            ;; If it does, we should add all it's properties to the list!
            (when (member comp-name company-meta-net--namespaces)
              (let ((scope
                     (list (cons (meta-net-type-methods xml type)    "(Method)")
                           (cons (meta-net-type-fields xml type)     "(Field)")
                           (cons (meta-net-type-events xml type)     "(Event)")
                           (cons (meta-net-type-properties xml type) "(Property)"))))
                (dolist (data scope)
                  (company-meta-net--add-tag-data (car data) (cdr data)))))))))
    (ht-keys company-meta-net--candidates)))

(defun company-meta-net--annotation (candidate)
  "Return annotation for CANDIDATE."
  (if company-meta-net-display-annotation
      (nth 0 (ht-get company-meta-net--candidates candidate))
    ""))

(defun company-meta-net--doc-buffer (candidate)
  "Return document for CANDIDATE."
  (company-doc-buffer
   (if company-meta-net-display-document
       (nth 1 (ht-get company-meta-net--candidates candidate))
     "")))

;;;###autoload
(defun company-meta-net (command &optional arg &rest ignored)
  "Company backend for VS C# project.

Arguments COMMAND, ARG and IGNORED are standard arguments from `company-mode`."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-meta-net))
    (prefix (company-meta-net--prefix))
    (candidates (company-meta-net--candidates))
    (annotation (company-meta-net--annotation arg))
    (doc-buffer (company-meta-net--doc-buffer arg))))

(provide 'company-meta-net)
;;; company-meta-net.el ends here
