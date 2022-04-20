;;; meta-net.el --- Parse .NET assembly's XML  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Shen, Jen-Chieh
;; Created date 2021-06-24 21:17:03

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Parse .NET assembly's XML
;; Keyword: assembly xml utility
;; Version: 1.1.0
;; Package-Version: 20220103.1609
;; Package-Commit: 6f54b36af376faee07591fc8b6a26631fa9e9932
;; Package-Requires: ((emacs "25.1") (ht "2.3") (f "0.20.0") (s "1.12.0"))
;; URL: https://github.com/emacs-vs/meta-net

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
;; Parse .NET assembly's XML.
;;

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'subr-x)

(require 's)
(require 'f)
(require 'ht)

(defgroup meta-net nil
  "Parse .NET assembly's XML."
  :prefix "meta-net-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/meta-net"))

(defconst meta-net--tag-method "M:"
  "Tag represent methods declaration.")

(defconst meta-net--tag-type "T:"
  "Tag represent types (enum, class, interface) declaration.")

(defconst meta-net--tag-field "F:"
  "Tag represent fields.")

(defconst meta-net--tag-property "P:"
  "Tag represent properties declaration.")

(defconst meta-net--tag-event "E:"
  "Tag represent events.")

(defconst meta-net--tag-unknown "?:"
  "Tag represent unknown.")

(defvar-local meta-net-csproj-current nil
  "File path points to current csproj file.

Please use this with variable `meta-net-csproj' to access data you need.

Do not modified this buffer, unless you have to.")

(defvar meta-net--possible-csproj nil
  "Record a list of possible csporj.

The new SDK based csproj format on longer needs to `includes` all source (.cs)
files, hence we need this variable to guess the possible csproj file.

See references Pt 2 in README file for more information.")

(defvar meta-net-csproj (ht-create)
  "Mapping of all csproj file entries.

Store data in (path . hash-table); hash-table are data defined in csproj.

See function `meta-net--parse-csproj-xml' to get more information.")

(defvar meta-net-xml (ht-create)
  "Mapping of all assembly xml files to it's data in hash table.

Store data in (path . hash-table); hash-table are data defined in assembly xml.")

(defvar meta-net-show-log t
  "Show the log message from this package.")

(defvar meta-net-show-debug nil
  "Show the debug message from this package.")

;;
;; (@* "Util" )
;;

(defun meta-net-log (fmt &rest args)
  "Log message like function `message' with same argument FMT and ARGS."
  (when meta-net-show-log
    (let (message-log-max) (apply 'message fmt args))))

(defun meta-net-debug (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when meta-net-show-debug (apply 'message fmt args)))

(defun meta-net--project-current ()
  "Return the current project root."
  (cdr (project-current)))

(defun meta-net--walk-path (path fnc &optional start-path)
  "Walk through PATH and execute FNC.

Argument START-PATH should be sub directory from PATH."
  (let* ((lst (f-split path)) (lst-start (when start-path (f-split start-path)))
         (len (length lst)) (len-start (length lst-start))
         (index (if start-path (1- len-start) 0))
         (current (if start-path start-path (nth index lst)))
         break)
    (while (and (not break) (< index len))
      (when (file-directory-p current)
        (setq break (funcall fnc current)))
      (setq index (1+ index))
      (when (< index len)
        (setq current (f-slash (f-join current (nth index lst))))))))

;;
;; (@* "Core" )
;;

(defun meta-net--grab-define-constants (project-node)
  "Return list of string that are define constants.

See the caller function `meta-net--parse-csproj-xml' for argument PROJECT-NODE."
  (let ((project-groups (xml-get-children project-node 'PropertyGroup))
        constants current)
    (dolist (project-group project-groups)
      (setq current (car (xml-get-children project-group 'DefineConstants)))
      (when current
        (setq current (nth 2 current)
              current (split-string current ";")
              constants (append constants current))))
    constants))

(defun meta-net--grab-includes (project-node project)
  "Return a list of path are are project source files.

See the caller function `meta-net--parse-csproj-xml' for arguments PROJECT-NODE
and PROJECT."
  (let ((item-groups (xml-get-children project-node 'ItemGroup))
        includes compiles attr-include)
    (dolist (item-group item-groups)
      (setq compiles (xml-get-children item-group 'Compile))
      (dolist (compile compiles)
        (setq attr-include (xml-get-attribute compile 'Include))  ; this is the source path
        (push (f-join project attr-include) includes)))
    (reverse includes)))

(defun meta-net--grab-assembly-xml (project-node project)
  "Return a list of path that are assembly xml.

See the caller function `meta-net--parse-csproj-xml' for arguments PROJECT-NODE
and PROJECT."
  (let ((item-groups (xml-get-children project-node 'ItemGroup))
        refs hint-path xml)
    (dolist (item-group item-groups)
      (setq refs (xml-get-children item-group 'Reference))
      (dolist (ref refs)
        (setq hint-path (nth 2 (car (xml-get-children ref 'HintPath))))
        (unless (file-exists-p hint-path)  ; Convert relative path to absolute path
          (setq hint-path (f-join project hint-path)))
        (setq hint-path (f-swap-ext hint-path "xml"))
        (when (file-exists-p hint-path)  ; file must exists
          (meta-net-create-entry-xml hint-path)
          (push hint-path xml))))
    (reverse xml)))

(defun meta-net--parse-csproj-xml (path)
  "Parse a csproj xml from PATH and return data in hash table.

Hash table includes these following keys,

   * constants - A list of define constans
   * includes  - A list of included source file
   * xml       - A list of assembly xml path

You can access these data through variable `meta-net-csproj'."
  (let* ((result (ht-create)) (parse-tree (xml-parse-file path))
         (project-node (assq 'Project parse-tree))
         (project (f-parent path))
         constants includes xml)
    (setq constants (meta-net--grab-define-constants project-node)
          includes (meta-net--grab-includes project-node project)
          xml (meta-net--grab-assembly-xml project-node project))
    (ht-set result 'constants constants)  ; add `constants' it to data
    (ht-set result 'includes includes)    ; add `includes' it to data
    (ht-set result 'xml xml)              ; add `xml' it to data
    result))

(defun meta-net--find-tag (name)
  "Find the tag from NAME."
  (cond ((string-match-p meta-net--tag-type name)     'type)
        ((string-match-p meta-net--tag-method name)   'method)
        ((string-match-p meta-net--tag-field name)    'field)
        ((string-match-p meta-net--tag-event name)    'event)
        ((string-match-p meta-net--tag-property name) 'property)
        ((string-match-p meta-net--tag-unknown name)  'unknown)
        (t nil)))

(defun meta-net--find-tag-string (name)
  "Find the tag string from NAME.

The value should be one of the follwin string, M:/T:/F:/P:/E:."
  (cl-case (meta-net--find-tag name)
    (type     meta-net--tag-type)
    (method   meta-net--tag-method)
    (field    meta-net--tag-field)
    (event    meta-net--tag-event)
    (property meta-net--tag-property)
    (unknown  meta-net--tag-unknown)
    (t "")))

(defun meta-net--rip-tag-name (name)
  "Rip off the tag from NAME.

For instnace, name `T:some-value` to `some-value`."
  (s-replace (meta-net--find-tag-string name) "" name))

(defun meta-net--subset-data (hashtable comp-name data tag)
  "Update the subset data.

Argument HASHTABLE is the root of parent node.  Arguments COMP-NAME and
DATA are pair data, in key and value.  Argument TAG is a symbol represents
the prefix name."
  (if (hash-table-p hashtable) (ht-set hashtable comp-name data)
    ;; Variable HASHTABLE is created inside `meta-net--grab-xml-members'
    ;; function; if it's missing, means there is invalid prefix ?: in the
    ;; xml document.
    ;;
    ;; You can often ignore this error, unless it appears multiple times
    ;; in a xml file.
    (message "[WARNING] Parent missing in assembly xml `%s`: %s" tag comp-name)))

(defun meta-net--grab-xml-members (doc-node)
  "Return members data from assembly xml.

Argument DOC-NODE is the root from assembly xml file."
  (let* ((result (ht-create))
         (members-node (car (xml-get-children doc-node 'members)))
         (members (xml-get-children members-node 'member))
         tag            ; current tag
         name-no-tag    ; name without the tag
         comp-name      ; actual name of the component after `.` (last)
         type-name      ; we use this as a key
         type-data      ; data from current `type`
         ;; -- SUBSETS -------------------------------------------------
         methods-data     ; current methods data, subset of type-data
         fields-data      ; current fields data, subset of type-data
         events-data      ; current events data, subset of type-data
         properties-data  ; current properties data, subset of type-data
         ;; ------------------------------------------------------------
         name summary-node para summary)
    (dolist (member members)
      (meta-net-debug "\f")
      (meta-net-debug "%s" member)
      ;; Get all necessary information from current `member` group
      (setq name (xml-get-attribute member 'name)
            tag (meta-net--find-tag name)
            name-no-tag (meta-net--rip-tag-name name)
            comp-name (s-replace (concat (or type-name "") ".") "" name-no-tag)
            summary-node (car (xml-get-children member 'summary))
            summary (nth 2 summary-node)
            para (nth 3 summary-node))
      (when para (setq summary (nth 2 para)))
      (when summary (setq summary (string-trim summary)))
      (meta-net-debug "---------")
      (meta-net-debug "name: %s" name)
      (meta-net-debug "summary: `%s`" summary)
      (cl-case tag
        (type  ; Type is the root of the tree
         (setq type-name name-no-tag
               type-data (ht-create)
               methods-data (ht-create) fields-data (ht-create)
               events-data (ht-create) properties-data (ht-create))
         (ht-set result type-name type-data)
         (ht-set type-data 'summary summary)
         (ht-set type-data 'methods methods-data)
         (ht-set type-data 'fields fields-data)
         (ht-set type-data 'events events-data)
         (ht-set type-data 'properties properties-data))
        (method
         (let* ((data (ht-create)) (params (xml-get-children member 'param))
                params-data param-name param-desc
                (returns-node (car (xml-get-children member 'returns)))
                (returns-desc (nth 2 returns-node))
                (returns-para (nth 3 returns-node)))
           (when (listp returns-para) (setq returns-desc (nth 2 returns-para)))
           (dolist (param params)
             (setq param-name (xml-get-attribute param 'name)
                   param-desc (nth 2 param))
             (meta-net-debug "  - name:   %s" param-name)
             (meta-net-debug "    desc:   %s" param-desc)
             (meta-net-debug "    return: %s" returns-desc)
             (push (cons param-name param-desc) params-data))
           (ht-set data 'summary summary)
           (ht-set data 'params (reverse params-data))
           (ht-set data 'returns returns-desc)
           (meta-net--subset-data methods-data comp-name data tag)))
        (field
         (let ((data (ht-create)))
           (ht-set data 'summary summary)
           (meta-net--subset-data fields-data comp-name data tag)))
        (event
         (let ((data (ht-create)))
           (ht-set data 'summary summary)
           (meta-net--subset-data events-data comp-name data tag)))
        (property
         (let ((data (ht-create)))
           (ht-set data 'summary summary)
           (meta-net--subset-data properties-data comp-name data tag)))
        (unknown
         ;; TODO: What should we do for unknown tag?
         (meta-net-debug "Detect unkown tag `%s`, name `%s`" meta-net--tag-unknown name-no-tag))))
    result))

(defun meta-net--parse-assembly-xml (path)
  "Parse a assembly (dll) xml from PATH and return data in hash table.

Hash table includes these following keys,

   * assembly  - Name of the assembly xml
   * data      - Hash table that use `type` key

You can access these data through variable `meta-net-xml'."
  (let* ((result (ht-create))
         (parse-tree (xml-parse-file path))
         (doc-node (assq 'doc parse-tree))
         (assembly (car (xml-get-children doc-node 'assembly)))
         data)
    (when assembly (ht-set result 'assembly (or assembly (f-base path))))
    (setq data (meta-net--grab-xml-members doc-node))
    (ht-set result 'data data)
    result))

;;;###autoload
(defun meta-net-read-project (&optional force)
  "Read .NET csproj from current project.

If argument FORCE is non-nil, refresh cache and rebuild data cleanly.

P.S. Please call the function under a project."
  (when force (setq meta-net-csproj-current nil))
  (setq meta-net--possible-csproj nil)
  (if meta-net-csproj-current
      (user-error "Data has been built, pass FORCE with t to rebuild")
    (let ((project (meta-net--project-current)) (path (f-parent (buffer-file-name)))
          csprojs)
      (if (not project) (user-error "Path is not under project root: %s" path)
        (meta-net--walk-path
         path
         (lambda (current)  ; `current` is the path of walking path
           (setq csprojs (f--files current (equal (f-ext it) "csproj")))
           (when csprojs  ; found csproj files in `current' directory
             (setq meta-net--possible-csproj (append meta-net--possible-csproj csprojs))
             (meta-net-create-entry-csproj csprojs)))
         project)))
    (meta-net-build-data force)))

(defun meta-net-create-entry-csproj (csprojs)
  "Create new csproj entry from current buffer.

Argument CSPROJS is a list of csproj files for use to create."
  (dolist (entry csprojs)
    (unless (ht-get meta-net-csproj entry)
      (meta-net-log "Create csproj entry: `%s`" entry)
      (ht-set meta-net-csproj entry nil))))

(defun meta-net-create-entry-xml (path)
  "Create new xml entry (PATH) from current buffer.

P.S. Use this carefully, this will overwrite the existing key with null."
  (meta-net-log "Create xml entry: `%s`" path)
  (ht-set meta-net-xml path nil))

(defun meta-net--find-current-csproj ()
  "Return a csproj path that includes the current buffer file."
  (let ((csprojs (ht-keys meta-net-csproj)) csproj (csproj-index 0)
        sources source source-index
        (target (buffer-file-name)) target-csproj
        break)
    (while (and (not break) (< csproj-index (length csprojs)))
      (setq csproj (nth csproj-index csprojs)
            csproj-index (1+ csproj-index)
            sources (meta-net-includes csproj)
            source-index 0)
      (while (and (not break) (< source-index (length sources)))
        (setq source (nth source-index sources)
              source-index (1+ source-index))
        (when (string= target source)
          (setq target-csproj csproj
                break t))))
    target-csproj))

(defun meta-net-build-data (&optional force)
  "Read all csproj files and read all assembly xml files to usable data.

If argument FORCE is non-nil, clean and rebuild."
  (let ((built t))
    ;; Access csproj to get assembly information including the xml path
    (let ((keys-csproj (ht-keys meta-net-csproj)) result)
      (dolist (key keys-csproj)                              ; key, is csproj path
        (when (or force (not (ht-get meta-net-csproj key)))  ; if it hasn't build, build it
          (meta-net-log "Build csproj data: `%s`" key)
          (setq result (meta-net--parse-csproj-xml key))     ; start building data
          (ht-set meta-net-csproj key result)
          (setq built nil))))
    ;; Find csproj for current buffer file
    (setq meta-net-csproj-current (meta-net--find-current-csproj))
    ;; This mean the current source file is not included inside the any
    ;; csproj file. This can be following conditions,
    ;;
    ;;   1. csproj and solution are not built correctly, try rebuild it
    ;;      using Visual Studio IDE (not VSCode)
    ;;
    ;;   2. The source file is not added to csproj file but exists under
    ;;      the project directory, add the source file to csproj from
    ;;      Visual Studio IDE or edit csproj your self
    ;;
    ;;   3. Not under a valid Visual Studio IDE C# project
    ;;
    ;;   4. The new SDK based csproj no longer needs to include all source
    ;;      (.cs) files.
    (unless meta-net-csproj-current
      (cond ((null meta-net--possible-csproj)
             (user-error "No csproj found in parent directory, make sure are under a valid VS C# project"))
            ;; Automatically pick the first one if that's the only choice
            ((= 1 (length meta-net--possible-csproj))
             (setq meta-net-csproj-current (nth 0 meta-net--possible-csproj)))
            ;; Else, we ask the user for all possible candidates
            (t (setq meta-net-csproj-current (completing-read "Select csproj: " meta-net--possible-csproj)))))
    (setq meta-net--possible-csproj nil)  ; you no longer need this, clean it
    ;; Build assembly xml data to cache
    (let ((keys-xml (ht-keys meta-net-xml)) result)
      (dolist (key keys-xml)                                ; key, is xml path
        (when (or force (not (ht-get meta-net-xml key)))    ; if it hasn't build, build it
          (meta-net-log "Build assembly xml data: `%s`" key)
          (setq result (meta-net--parse-assembly-xml key))  ; start building data
          (ht-set meta-net-xml key result)
          (setq built nil))))
    (if built (message "Everything up to date, no need to rebuild")
      (message "Done rebuild solution for project: `%s`" meta-net-csproj-current))))

;;
;; (@* "CsProj" )
;;

(defun meta-net--get-csproj (path key)
  "Return csproj data by it's PATH with KEY."
  (if-let ((data (ht-get meta-net-csproj path)))
      (ht-get data key)
    (user-error "CsProj data not found, %s => `%s`" key path)))

(defun meta-net-define-constants (path)
  "Return define constants from a csproj PATH file."
  (meta-net--get-csproj path 'constants))

(defun meta-net-includes (path)
  "Return includes source file from a csproj PATH file."
  (meta-net--get-csproj path 'includes))

(defun meta-net-csproj-xmls (path)
  "Return list of assembly xml files.

Argument PATH is the csproj path that points to it file."
  (meta-net--get-csproj path 'xml))

;;
;; (@* "Xmls" )
;;

(defun meta-net--get-xml (path key)
  "Return xml data by it's PATH with KEY."
  (if-let ((data (ht-get meta-net-xml path)))
      (ht-get data key)
    (user-error "Xml data not found, %s => `%s`" key path)))

(defun meta-net-xml-assemly-name (path)
  "Return the name of the assembly.

Argument PATH is the path points to assembly xml file."
  (meta-net--get-xml path 'assembly))

(defun meta-net-xml-data (path)
  "Return the data of the assembly.

Argument PATH is the path points to assembly xml file."
  (meta-net--get-xml path 'data))

;;
;; (@* "Types Data" )
;;

(defun meta-net-xml-types (xml)
  "Return all types from assembly XML."
  (if-let ((data (meta-net-xml-data xml)))
      (ht-keys data)
    (user-error "Xml not found, %s" xml)))

(defun meta-net--type-get (xml type)
  "Return TYPE data from assembly XML in hash table."
  (if-let ((data (meta-net-xml-data xml)))
      (ht-get data type)
    (user-error "Assembly type not found, %s" type)))

(defun meta-net--type-data-get (xml type key)
  "Return TYPE data from assembly XML.

Argument XML is it's path, and TYPE is the type data fomr the assembly xml.

Argument KEY should be one of the tag, `methods`, `fields`, etc."
  (when-let ((data (meta-net--type-get xml type)))
    (ht-get data key)))

(defun meta-net-type-methods (xml type)
  "Return all methods (hashtable) data.

See function `meta-net--type-data-get' for arguments XML and TYPE."
  (meta-net--type-data-get xml type 'methods))

(defun meta-net-type-fields (xml type)
  "Return all fields (hashtable) data.

See function `meta-net--type-data-get' for arguments XML and TYPE."
  (meta-net--type-data-get xml type 'fields))

(defun meta-net-type-events (xml type)
  "Return all events (hashtable) data.

See function `meta-net--type-data-get' for arguments XML and TYPE."
  (meta-net--type-data-get xml type 'events))

(defun meta-net-type-properties (xml type)
  "Return all properties (hashtable) data.

See function `meta-net--type-data-get' for arguments XML and TYPE."
  (meta-net--type-data-get xml type 'properties))

;;
;; (@* "Tags Data" )
;;

(defun meta-net-type-summary (xml type)
  "Return summary from TYPE.

See function `meta-net--type-data-get' for arguments XML and TYPE."
  (meta-net--type-data-get xml type 'summary))

(provide 'meta-net)
;;; meta-net.el ends here
