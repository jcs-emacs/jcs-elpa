;;; meta-view.el --- View metadata from .NET assemblies  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Shen, Jen-Chieh
;; Created date 2021-06-24 14:01:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: View metadata from .NET assemblies
;; Keyword: assembly metadata source
;; Version: 0.1.0
;; Package-Version: 20220103.1609
;; Package-Commit: 7227835ceb69920b08c46ee0ea367e68adaf8f23
;; Package-Requires: ((emacs "26.1") (csharp-mode "0.11.0") (meta-net "1.1.0") (ht "2.3") (f "0.20.0"))
;; URL: https://github.com/emacs-vs/meta-view

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
;; View metadata from .NET assemblies.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'csharp-mode)
(require 'ht)
(require 'f)
(require 'meta-net)

(defgroup meta-view nil
  "View metadata from .NET assemblies."
  :prefix "meta-view-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/meta-view"))

(defcustom meta-view-active-modes
  '(csharp-mode csharp-tree-sitter-mode)
  "Major modes that allow to view metadata source."
  :type 'list
  :group 'meta-view)

(defcustom meta-view-after-insert-hook nil
  "Hooks run after buffer is inserted to display view."
  :type 'hook
  :group 'meta-view)

(defcustom meta-view-display-function #'switch-to-buffer
  "Function to display reference data."
  :type 'function
  :group 'meta-view)

(defconst meta-view--templates-dir
  (concat (file-name-directory load-file-name) "templates/")
  "Templates path for package `meta-view'.")

(defconst meta-view-reference-alist
  '(("Boolean" . "bool")
    ("Int32" . "int")
    ("Integer" . "int")
    ("String"  . "string")
    ("Single"   . "float"))
  "List of xml keywords to C# readable keywords.")

(defconst meta-view--buffer-name "%s <from metadata>"
  "Buffer name to display metadata.")

(defvar meta-view--buffers nil
  "List of buffers being view.")

(defvar-local meta-view--xmls nil
  "Cache records a list of assembly xml file path.")

(defvar meta-view-show-debug nil
  "Show the debug message from this package.")

(defvar meta-view--namespaces nil
  "A list of used namespaces in assembly reference file.

This is use only when function `meta-view' is called.")

;;
;; (@* "Util" )
;;

(defun meta-view-debug (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when meta-view-show-debug (apply 'message fmt args)))

(defun meta-view--inside-comment-or-string-p ()
  "Return non-nil if it's inside comment or string."
  (or (nth 4 (syntax-ppss)) (nth 8 (syntax-ppss))))

(defun meta-view--contain-list-string-regexp (in-list in-str)
  "Return non-nil if IN-STR is listed in IN-LIST.

This function uses `string-match-p'."
  (cl-some (lambda (elm) (string-match-p in-str (regexp-quote elm))) in-list))

(defun meta-view--line-empty-p ()
  "Return non-nil, if current line empty."
  (string-empty-p (thing-at-point 'line)))

(defun meta-view--get-string-from-file (path)
  "Return PATH file content."
  (if (file-exists-p path)
      (with-temp-buffer (insert-file-contents path) (buffer-string))
    ""))

(defun meta-view--re-seq (regexp string)
  "Get a list of all REGEXP match in a STRING."
  (save-match-data
    (let ((pos 0) matches)
      (while (string-match regexp string pos)
        (push (match-string 1 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun meta-view--add-buffer (buffer)
  "Add BUFFER to view list."
  (push buffer meta-view--buffers)
  (cl-delete-duplicates meta-view--buffers)
  (setq meta-view--buffers
        (cl-remove-if-not (lambda (buf) (buffer-live-p buf)) meta-view--buffers)))

(defmacro meta-view--with-buffer (name &rest body)
  "Execute BODY inside the metadata displayed buffer with NAME."
  (declare (indent 1) (debug t))
  `(let ((buf-name (format meta-view--buffer-name ,name)))
     (with-current-buffer (get-buffer-create buf-name)
       (meta-view--add-buffer (current-buffer))
       (delay-mode-hooks (funcall 'csharp-mode))
       (ignore-errors (font-lock-ensure))
       (buffer-disable-undo)
       (let (buffer-read-only)
         (erase-buffer)
         (progn ,@body)
         (run-hooks 'meta-view-after-insert-hook))
       (setq buffer-read-only t))))

;;
;; (@* "Xmls" )
;;

(defun meta-view--all-xmls (&optional refresh)
  "Return full list of assembly xml files.

If REFRESH is non-nil, refresh cache once."
  (when (or refresh (null meta-view--xmls))
    (setq meta-view--xmls (meta-net-csproj-xmls meta-net-csproj-current))
    (cl-delete-duplicates meta-view--xmls))
  meta-view--xmls)

;;
;; (@* "Insertion" )
;;

(defun meta-view--insert-summary (summary)
  "Insert SUMMARY to meta source buffer."
  (when summary
    (insert "\n")
    (insert "//\n")
    (insert "// Summary:\n")
    (insert "//     " summary)))

(defun meta-view--insert-type-summary (summary type)
  "Insert type SUMMARY, what's above class/enum/interface.

We use argument TYPE to raise accuracy while search for position."
  (save-excursion
    (goto-char (point-min))
    (search-forward (format "namespace %s" type) nil t)
    (forward-line 1)
    (end-of-line)
    (meta-view--insert-summary summary)))

(defun meta-view--insert-namespace (namespace type)
  "Insert NAMESPACE string below syntax `endregion'.

We use argument TYPE to raise accuracy while search for position."
  (save-excursion
    (goto-char (point-min))
    (search-forward (format "namespace %s" type) nil t)
    (forward-line -1)
    (save-excursion
      (forward-line -1)
      (unless (string-match-p "using" (thing-at-point 'line))
        (end-of-line)
        (insert "\n")))
    (insert "using " namespace ";\n")))

(defun meta-view--insert-methods-params (params)
  "Insert all PARAMS document string."
  (let ((first t))
    (dolist (param params)
      (insert "\n")
      (if first (setq first nil) (insert "//\n") )
      (insert "//   " (car param) ":\n")
      (if (cdr param)
          (insert "//     " (cdr param))
        (backward-delete-char 1)))))

(defun meta-view--insert-methods-returns (desc)
  "Insert return (DESC) document string."
  (when desc
    (insert "\n")
    (insert "//\n")
    (insert "// Returns:\n")
    (insert "//     " desc)))

(defun meta-view--grab-namespace (method)
  "Grab namespace from METHOD string."
  (meta-view--re-seq "[(.,]\\([a-zA-Z0-9._-]+\\)[.]" method))

(defun meta-view--insert-methods (methods type)
  "Insert METHODS data.

Argument TYPE is used replace #ctor."
  (let ((keys (ht-keys methods)) item summary params returns namespaces
        was-ctor current-ctor done-ctor)
    ;; We sort the constructor infront
    (setq keys (sort keys (lambda (key &rest _)(string-match-p "#ctor" key))))
    (dolist (key keys)
      (setq item (ht-get methods key)
            summary (ht-get item 'summary)
            params (ht-get item 'params)
            returns (ht-get item 'returns))
      (unless done-ctor  ; Insert line break for ctors
        (setq current-ctor (string-match-p "#ctor" key))
        (when (and was-ctor (not current-ctor))
          (insert "\n")
          (setq done-ctor t))
        (setq was-ctor current-ctor))
      ;; Insert document string
      (meta-view--insert-summary summary)
      (insert "\n")
      (insert "//\n")
      (insert "// Parameters:")
      (meta-view--insert-methods-params params)
      (meta-view--insert-methods-returns returns)
      (insert "\n")
      ;; Grab namespaces
      (setq namespaces (meta-view--grab-namespace key)
            ;; Add to global namespaces, so we can inserted on top later on
            meta-view--namespaces (append meta-view--namespaces namespaces))
      (dolist (ns namespaces)  ; Rip off namespace!
        (setq key (s-replace (concat ns ".") "" key)))
      ;; Replace keywords
      (setq key (s-replace "#ctor" type key)
            key (s-replace "," ", " key))
      ;; Add parameter names after each argument types
      (with-temp-buffer
        (insert key)
        (goto-char (point-min))
        (dolist (param params)
          (when (re-search-forward "[,)]" nil t)
            (forward-char -1)
            (insert " " (car param))
            (forward-char 1)))
        (setq key (buffer-string)))
      ;; Replace xml keywords to C# keywords
      (dolist (pair meta-view-reference-alist)
        (let ((keyword (car pair)) (replace (cdr pair)))
          (setq key (s-replace-regexp (format "\\_<%s\\_>" keyword) replace key t))))
      ;; If missing (), add it
      (unless (string-match-p ")" key) (setq key (concat key "()")))
      ;; Lastly, insert it!
      (insert "public var " key ";"))))

(defun meta-view--insert-fields (fields)
  "Insert FIELDS data."
  (let ((keys (ht-keys fields)) item summary)
    (dolist (key keys)
      (setq item (ht-get fields key)
            summary (ht-get item 'summary))
      (meta-view--insert-summary summary)
      (insert "\n")
      (insert key ","))))

(defun meta-view--insert-events (events)
  "Insert EVENTS data."
  (let ((keys (ht-keys events)) item summary)
    (dolist (key keys)
      (setq item (ht-get events key)
            summary (ht-get item 'summary))
      (meta-view--insert-summary summary)
      (insert "\n")
      (insert "public event " key ";"))))

(defun meta-view--insert-properties (properties)
  "Insert PROPERTIES data."
  (let ((keys (ht-keys properties)) item summary)
    (dolist (key keys)
      (setq item (ht-get properties key)
            summary (ht-get item 'summary))
      (meta-view--insert-summary summary)
      (insert "\n")
      (insert "public var " key ";"))))

;;
;; (@* "Core" )
;;

(defun meta-view--fill-info (template-str xml namespace name)
  "Replace information for XML, NAMESPACE and NAME.

TEMPLATE-STR is the string read from `templates' folder."
  ;; These keys (prefix with `dollar` sign) are defined in template files
  (setq template-str (s-replace "$ASSEMBLY_INFO" (f-base xml) template-str)
        template-str (s-replace "$DLL_PATH" (f-swap-ext xml "dll") template-str)
        template-str (s-replace "$NAMESPACE" namespace template-str)
        template-str (s-replace "$NAME" name template-str))  ; class/enum/interface name
  template-str)

(defun meta-view--choose-template (declare-type)
  "Return the path of the template by DECLARE-TYPE."
  (f-join meta-view--templates-dir
          (cl-case declare-type
            (class "class.cs")
            (enum "enum.cs")
            (interface "interface.cs"))))

(defun meta-view--find-declare-type (xml type)
  "Find the declaration type.

See function `meta-net--type-data-get' for arguments XML and TYPE."
  (let* ((methods (meta-net-type-methods xml type))
         (fields (meta-net-type-fields xml type))
         (events (meta-net-type-events xml type))
         (properties (meta-net-type-properties xml type))
         (methods-len (length (ht-keys methods)))
         (fields-len (length (ht-keys fields)))
         (events-len (length (ht-keys events)))
         (properties-len (length (ht-keys properties)))
         (no-methods (zerop methods-len))
         (no-fields (zerop fields-len))
         (no-events (zerop events-len))
         (no-properties (zerop properties-len)))
    (cond ((and no-methods (not no-fields) no-events no-properties)
           ;; If fields is the only tag that appears then it has higher chance
           ;; to be an enumerator
           'enum)
          ((and (not no-methods) no-fields no-events no-properties)
           ;; Interface only contains methods
           'interface)
          ((and (not no-methods) (not no-properties))
           ;; Class can have everything
           'class)
          ;; In case, return unknown
          (t
           (user-error "Detect unknown declaration: %s %s" type
                       (list methods-len fields-len events-len properties-len))
           'unknown))))

(defun meta-view--matching-data (tag-data target)
  "Return non-nil, if TARGET can be found in TAG-DATA.

Argument TAG-DTA is in hash-table and it stores all data in tag categoray.

Tag are `methods`, `fields`, `events` and `properties`."
  (let ((names (ht-keys tag-data)))
    (meta-view--contain-list-string-regexp names target)))

(defun meta-view--find-matching (xml type target)
  "Return non-nil, if TARGET can be found xml data.

See function `meta-net--type-data-get' for arguments XML and TYPE."
  (cl-some (lambda (tag-data)
             (meta-view--matching-data tag-data target))
           (list (meta-net-type-methods xml type)
                 (meta-net-type-fields xml type)
                 (meta-net-type-events xml type)
                 (meta-net-type-properties xml type))))

(defun meta-view--match-name (name)
  "Return non-nil, if NAME exists inside the buffer.

The name should similar to namepsace syntax, `System.Collections.UI`, etc."
  (let* ((names (split-string name "\\."))
         (match t) keyword (index 0) (len (length names)) last-item)
    (save-excursion
      ;; Incremental search from the start of the buffer to eliminate
      ;; some of the possible candidates.
      (goto-char (point-min))
      (while (and match (< index len))
        (setq keyword (nth index names)
              index (1+ index)
              last-item (= len index))
        (setq match (re-search-forward
                     (if last-item keyword (format "%s[ \t\n]*[.;]" keyword))
                     nil t))
        (when (meta-view--inside-comment-or-string-p)
          (setq match t))))
    (integerp match)))

;;;###autoload
(defun meta-view-at-point ()
  "View metadata at current point."
  (interactive)
  (meta-view (thing-at-point 'symbol)))

;;;###autoload
(defun meta-view (name)
  "View metadata by NAME."
  (unless (memq major-mode meta-view-active-modes)
    (user-error "Invalid major-mode to view metadata, %s" major-mode))
  (when (meta-view--inside-comment-or-string-p)
    (user-error "View under comment or string is not allow"))
  (unless (stringp name) (user-error "Invalid name to view metadata, %s" name))
  (unless meta-net-csproj-current (meta-net-read-project))
  (let* ((xmls (meta-view--all-xmls))  ; Get the list of xml files from current project
         (xmls-len (length xmls))      ; length of the xmls
         (xml-index 0)                 ; index search through all `xmls`
         (project meta-net-csproj-current)
         meta-view--namespaces  ; list namespace that displays on top, under `endregion'
         xml             ; current xml path as key
         break           ; flag to stop
         type            ; xml assembly type
         comp-name       ; name of the type, the last component from the type
         splits          ; temporary list to chop namespace, use to produce `comp-name`
         decalre-type    ; guess the declaration type
         template        ; chosen template path
         template-str    ; template string, load it from `template`
         type-namespace
         type-summary    ; summary for type
         display-pt
         new-buffer)
    (while (and (not break) (< xml-index xmls-len))
      (setq xml (nth xml-index xmls)
            xml-index (1+ xml-index))
      (let* ((types (meta-net-xml-types xml))
             (types-len (length types))
             (type-index 0))
        (while (and (not break) (< type-index types-len))
          (setq type (nth type-index types)
                type-index (1+ type-index)
                splits (split-string type "\\.")
                comp-name (nth (1- (length splits)) splits))
          ;; Check if all namespaces exists in the buffer,
          (when (meta-view--match-name type)
            (meta-view-debug "\f")
            (meta-view-debug "xml: %s" xml)
            (meta-view-debug "Type: %s" type)
            (meta-view-debug "Name: %s" comp-name)
            (meta-view-debug "Declare: %s" decalre-type)
            (when (or (string= name comp-name)                   ; Viewing type data?
                      (meta-view--find-matching xml type name))  ; Viewing data under the type
              (setq break t
                    type-namespace (s-replace (concat "." comp-name) "" type)
                    decalre-type (meta-view--find-declare-type xml type)
                    template (meta-view--choose-template decalre-type)
                    template-str (meta-view--get-string-from-file template)
                    template-str (meta-view--fill-info template-str xml
                                                       type-namespace comp-name)
                    type-summary (meta-net-type-summary xml type))
              (meta-view--with-buffer comp-name
                (setq meta-net-csproj-current project  ; assign to current project
                      new-buffer (current-buffer))
                (insert template-str)

                ;; Insert reference summary
                (meta-view--insert-type-summary type-summary type-namespace)

                ;; Navigate to content keyword, and ready to insert content
                (goto-char (point-min))
                (search-forward "$CONTENT" nil t)
                (delete-region (1- (line-beginning-position)) (line-end-position))

                ;; Insert all reference data
                (setq display-pt (point))
                (meta-view--insert-methods (meta-net-type-methods xml type) comp-name)
                (unless (= display-pt (point)) (insert "\n") (setq display-pt (point)))
                (meta-view--insert-fields (meta-net-type-fields xml type))
                (unless (= display-pt (point)) (insert "\n") (setq display-pt (point)))
                (meta-view--insert-events (meta-net-type-events xml type))
                (unless (= display-pt (point)) (insert "\n") (setq display-pt (point)))
                (meta-view--insert-properties (meta-net-type-properties xml type))

                ;; Prepare for namespaces
                (setq meta-view--namespaces (delete-dups meta-view--namespaces)
                      ;; Remove the namespace that we are currently in
                      meta-view--namespaces (cl-remove-if (lambda (ns) (string= type-namespace ns))
                                                          meta-view--namespaces)
                      ;; Sort in alphabetic order
                      meta-view--namespaces (sort meta-view--namespaces #'string-lessp))
                ;; Insert used namespaces
                (dolist (namespace meta-view--namespaces)
                  (meta-view--insert-namespace namespace type-namespace))

                ;; Indent all before displaying
                (let ((inhibit-message t) message-log-max)
                  (ignore-errors (indent-region (point-min) (point-max))))

                ;; Display buffer for view
                (funcall meta-view-display-function new-buffer)

                ;; Finally, points to the target symbol `name`
                (progn  ; Search from namespace, avoid search above namespace
                  (goto-char (point-min))
                  (re-search-forward (format "\\_<namespace\\_> %s" type-namespace) nil t))
                ;; Search for the target symbol
                (re-search-forward (format "\\_<%s\\_>" name) nil t)
                (while (meta-view--inside-comment-or-string-p)  ; not allow in comment
                  (re-search-forward (format "\\_<%s\\_>" name) nil t))))))))
    new-buffer))

(provide 'meta-view)
;;; meta-view.el ends here
