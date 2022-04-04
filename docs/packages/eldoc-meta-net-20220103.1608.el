;;; eldoc-meta-net.el --- Eldoc support for meta-net  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Shen, Jen-Chieh
;; Created date 2021-07-12 23:32:13

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Eldoc support for meta-net
;; Keyword: eldoc c# dotnet sdk
;; Version: 0.1.0
;; Package-Version: 20220103.1608
;; Package-Commit: 8a70954ab09a7a4113696e545afc8588abc2a1e6
;; Package-Requires: ((emacs "26.1") (meta-net "1.1.0") (ht "2.3") (csharp-mode "1.0.2"))
;; URL: https://github.com/emacs-vs/eldoc-meta-net

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
;; Eldoc support for meta-net
;;

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'subr-x)

(require 'csharp-mode)
(require 'eldoc)
(require 'meta-net)
(require 'ht)

(defgroup eldoc-meta-net nil
  "Eldoc support for meta-net."
  :prefix "eldoc-meta-net-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/eldoc-meta-net"))

(defcustom eldoc-meta-net-display-summary nil
  "If non-nil, display summary if valid."
  :type 'boolean
  :group 'eldoc-meta-net)

;; These keywords are grab from `csharp-mode'
(defconst eldoc-meta-net--csharp-keywords
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

(defvar-local eldoc-meta-net--namespaces nil
  "Where store all the parsed namespaces.")

(defvar eldoc-meta-net-show-debug nil
  "Show the debug message from this package.")

;;
;; (@* "Util" )
;;

(defun eldoc-meta-net-debug (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when eldoc-meta-net-show-debug (apply 'message fmt args)))

(defun eldoc-meta-net--inside-comment-p ()
  "Return non-nil if it's inside comment."
  (nth 4 (syntax-ppss)))

(defun eldoc-meta-net--inside-comment-or-string-p ()
  "Return non-nil if it's inside comment or string."
  (or (eldoc-meta-net--inside-comment-p) (nth 8 (syntax-ppss))))

(defun eldoc-meta-net--recursive-count (regex string &optional start)
  "Count REGEX in STRING; return an integer value.

Optional argument START is use for recursive counting."
  (unless start (setq start 0))
  (if (string-match regex string start)
      (+ 1 (eldoc-meta-net--recursive-count regex string (match-end 0)))
    0))

;;
;; (@* "Xmls" )
;;

(defvar-local eldoc-meta-net--xmls nil
  "Cache records a list of assembly xml file path.")

(defun eldoc-meta-net--all-xmls (&optional refresh)
  "Return full list of assembly xml files.
If REFRESH is non-nil, refresh cache once."
  (when (or refresh (null eldoc-meta-net--xmls))
    (setq eldoc-meta-net--xmls (meta-net-csproj-xmls meta-net-csproj-current))
    (cl-delete-duplicates eldoc-meta-net--xmls))
  eldoc-meta-net--xmls)

;;
;; (@* "Core" )
;;

(defun eldoc-meta-net--grab-namespaces ()
  "Parsed namespaces from current buffer."
  (setq eldoc-meta-net--namespaces nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[,.:]*[ \t\n]*\\([a-z-A-Z0-9_-]+\\)[ \t\n]*[.;{]+" nil t)
      (save-excursion
        (forward-symbol -1)
        (unless (eldoc-meta-net--inside-comment-or-string-p)
          (when-let ((symbol (thing-at-point 'symbol)))
            (push symbol eldoc-meta-net--namespaces))))))
  (setq eldoc-meta-net--namespaces (reverse eldoc-meta-net--namespaces)
        eldoc-meta-net--namespaces (delete-dups eldoc-meta-net--namespaces)
        eldoc-meta-net--namespaces (cl-remove-if (lambda (namespace)
                                                   (member namespace eldoc-meta-net--csharp-keywords))
                                                 eldoc-meta-net--namespaces)))

(defun eldoc-meta-net--possible-function-point ()
  "This function get called infront of the opening curly bracket.

For example,

   SomeFunction<TypeA, TypeB>(a, b, c);
                             ^

This function also ignore generic type between < and >."
  (let ((start (point))
        (normal (save-excursion (forward-symbol -1) (point)))
        (generic
         (ignore-errors
           (save-excursion (search-backward ">" nil t)
                           (forward-char 1)
                           (forward-sexp -1)
                           (forward-symbol -1)
                           (point))))
        result search-pt)
    ;; Make sure the result is number to avoid error
    (setq normal (or normal (point))
          generic (or generic (point))
          result (min normal generic))
    (when (<= generic (line-beginning-position))
      (setq generic (point)))
    (goto-char result)  ; here suppose to be the start of the function name
    ;; We check to see if there is comma right behind the symbol
    (save-excursion
      (forward-symbol 1)
      (setq search-pt (point))
      (when (and (re-search-forward "[^,]*" nil t)
                 (string-empty-p (string-trim (buffer-substring search-pt (point)))))
        (setq result start)))
    (goto-char result)))

(defun eldoc-meta-net--function-name ()
  "Return the function name at point."
  (let* ((right 0) (left 0))
    (while (and (<= left right) (re-search-backward "\\((\\|)\\)" nil t))
      (if (equal (buffer-substring (point) (+ (point) 1)) "(")
          (setq left (+ left 1))
        (setq right (+ right 1))))
    (while (equal (buffer-substring (- (point) 1) (point)) " ")
      (goto-char (- (point) 1))))
  (save-excursion
    (eldoc-meta-net--possible-function-point)
    (unless (eldoc-meta-net--inside-comment-p) (thing-at-point 'symbol))))

(defun eldoc-meta-net--arg-boundaries ()
  "Return a list of cons cell represent arguments' boundaries.

The start boundary should either behind of `(` or `,`; the end boundary should
either infront of `,` or `)`.

For example, (^ is start; $ is end)

    Add(var1, var2, var3)
        ^  $ ^   $ ^    $

This function also get called infront of the opening curly bracket.  See
function `eldoc-meta-net--possible-function-point' for the graph."
  (let (bounds start (max-pt (save-excursion (forward-sexp 1))))
    (save-excursion
      (forward-char 1)
      (setq start (point))
      (while (re-search-forward "[,<{[()]" max-pt t)
        (pcase (string (char-before))
          ((or "," ")")
           (push (cons start (1- (point))) bounds)
           (setq start (point)))
          ((or "{" "(" "<" "[") (forward-char -1) (forward-sexp 1)))))
    (reverse bounds)))

(defun eldoc-meta-net-current-index (bounds point)
  "Return the index of current boundary from BOUNDS.

Argument POINT is the currnet check point."
  (cl-position-if
   (lambda (bound)
     (let ((start (car bound)) (end (cdr bound)))
       (and (<= start point) (<= point end))))
   bounds))

(defun eldoc-meta-net--match-name (type)
  "Return non-nil, if the TYPE match the current namespace list.

The argument TYPE is a list of namespace in string.  For instance,

   using System.Collections;  => '(System Collections)

We use this to eliminate not possible candidates."
  (let ((match t) (len (length type)) (index 0) item)
    (while (and match (< index len))
      (setq item (nth index type)
            index (1+ index)
            match (member item eldoc-meta-net--namespaces)))
    match))

(defun eldoc-meta-net--grab-data (function-name)
  "Return data that match FUNCTION-NAME."
  (eldoc-meta-net--grab-namespaces)        ; first grab the data from `meta-net'

  (let* ((xmls (eldoc-meta-net--all-xmls))  ; Get the list of xml files from current project
         (xmls-len (length xmls))           ; length of the xmls
         (xml-index 0)                      ; index search through all `xmls`
         xml          ; current xml path as key
         break        ; flag to stop
         type         ; xml assembly type
         comp-name    ; name of the type, the last component from the type
         splits       ; temporary list to chop namespace, use to produce `comp-name`
         namespaces
         result)
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
          (when (eldoc-meta-net--match-name namespaces)
            (eldoc-meta-net-debug "\f")
            (eldoc-meta-net-debug "xml: %s" xml)
            (eldoc-meta-net-debug "Type: %s" type)
            (eldoc-meta-net-debug "Name: %s" comp-name)
            (when-let* ((methods (meta-net-type-methods xml type))
                        (methods-keys (ht-keys methods)))
              (dolist (key methods-keys)  ; `key` is function name with arguments
                (when (string-match-p (format "\\_<%s\\_>" function-name) key)
                  (push (cons key (ht-get methods key)) result)
                  (setq break t))))))))
    result))

(defun eldoc-meta-net-function ()
  "Main eldoc entry."
  (save-excursion
    (when-let* ((start (point))
                (function-name (eldoc-meta-net--function-name))
                (arg-bounds (eldoc-meta-net--arg-boundaries))  ; list of cons cell
                (arg-index (eldoc-meta-net-current-index arg-bounds start))
                (arg-count (length arg-bounds))
                (methods (eldoc-meta-net--grab-data function-name)))
      (eldoc-meta-net-debug "Funtion name: %s" function-name)
      (eldoc-meta-net-debug "Arg Bounds: %s" arg-bounds)
      (eldoc-meta-net-debug "Arg Count: k%s" arg-count)
      ;; Find match arguments count, for function overloading
      (let ((index 0) (len (length methods)) data found
            name info match-arg-count match-bounds)
        (while (and (not found) (< index len))
          (setq data (nth index methods)
                index (1+ index)
                name (car data) info (cdr data)
                match-arg-count 0)
          (with-temp-buffer
            (insert name)
            (goto-char (point-min))
            (when (search-forward "(" nil t)
              (forward-char -1)
              (setq match-bounds (eldoc-meta-net--arg-boundaries)
                    match-arg-count (length match-bounds))
              ;; Notice that function overloading can has same argument count
              ;; but with different type.
              ;;
              ;; For instance,
              ;; ---
              ;;   OverloadingFunction(System.String)
              ;; ---
              ;; and,
              ;; ---
              ;;   OverloadingFunction(System.Type)
              ;; ---
              ;;
              ;; Since we cannot know the type from the uesr, we just pick one
              ;; that matches.
              (when (= match-arg-count arg-count)
                (setq found t)))))

        ;; Start display buffer
        (with-temp-buffer
          (delay-mode-hooks (funcall 'csharp-mode))
          (ignore-errors (font-lock-ensure))
          (insert name)
          (let ((params (ht-get info 'params)) (param-index (1- (length match-bounds)))
                param param-name param-summary)
            (dolist (bound (reverse match-bounds))     ; we replace from the back
              (delete-region (car bound) (cdr bound))  ; deletion for replacement
              (goto-char (car bound))                  ; navigate back to starting position
              (setq param (nth param-index params)
                    param-name (car param))
              (when (= param-index arg-index)    ; check if we need to highlight argument's name
                (setq param-summary (cdr param)  ; record target's (param) summary later insertion
                      ;; Highlight it!
                      param-name (propertize param-name 'face 'eldoc-highlight-function-argument)))
              ;; Make sure we don't add a space infront of (
              (insert (if (= param-index 0) "" " ") param-name)  ; insert replacement
              (setq param-index (1- param-index)))
            ;; Insert parameter's summary
            (when (and eldoc-meta-net-display-summary param-summary)
              (goto-char (point-max))
              (insert "\n\n" param-summary)))
          ;; Done, return it
          (buffer-string))))))

(defun eldoc-meta-net--turn-on ()
  "Start the `eldoc-meta-net' worker."
  (unless meta-net-csproj-current (meta-net-read-project))  ; read project
  (add-function :before-until (local 'eldoc-documentation-function) #'eldoc-meta-net-function)
  (eldoc-mode 1))

;;;###autoload
(defun eldoc-meta-net-enable ()
  "Turn on `eldoc-meta-net'."
  (interactive)
  (add-hook 'csharp-mode-hook #'eldoc-meta-net--turn-on)
  (add-hook 'csharp-tree-sitter-mode #'eldoc-meta-net--turn-on))

;;;###autoload
(defun eldoc-meta-net-disable ()
  "Turn off `eldoc-meta-net'."
  (interactive)
  (remove-hook 'csharp-mode-hook #'eldoc-meta-net--turn-on)
  (remove-hook 'csharp-tree-sitter-mode #'eldoc-meta-net--turn-on))

(provide 'eldoc-meta-net)
;;; eldoc-meta-net.el ends here
