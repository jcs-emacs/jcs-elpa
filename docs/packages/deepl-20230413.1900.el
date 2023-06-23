;;; deepl.el --- Elisp library for the DeepL API  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/deepl
;; Package-Version: 20230413.1900
;; Package-Commit: 436a0d479e31949945686b0fa328603df5ee5063
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (request "0.3.0"))
;; Keywords: comm deepl

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Elisp library for the DeepL API
;;

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)

(require 'request)

(defgroup deepl nil
  "Elisp library for the DeepL API."
  :prefix "deepl-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/deepl"))

;;
;;; Logger

(defvar deepl--show-log nil
  "Get more information from the program.")

(defun deepl--log (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when deepl--show-log
    (apply 'message fmt args)))

;;
;;; Request

(defvar deepl-key ""
  "Variable storing the deepl key or a function name to retrieve it.

The function should take no arguments and return a string containing the key.

A function, `deepl-key-auth-source', that retrieves the key from auth-source is
provided for convenience.")

(defcustom deepl-free t
  "Non-nil then use free version."
  :type 'boolean
  :group 'deepl)

(defcustom deepl-base-url (if deepl-free
                              "https://api-free.deepl.com/v2"
                            "https://api.deepl.com/v2")
  "The base URL for DeepL API requests."
  :type 'string
  :group 'deepl)

(defcustom deepl-parameters '()
  "The parameters for the DeepL request."
  :type 'list
  :group 'deepl)

;;;###autoload
(defun deepl-key-auth-source (&optional base-url)
  "Retrieve the DeepL API key from auth-source given a BASE-URL.

If BASE-URL is not specified, it defaults to `deepl-base-url'."
  (if-let ((auth-info (auth-source-search
                       :max 1
                       :host (url-host (url-generic-parse-url (or base-url deepl-base-url)))
                       :require '(:user :secret))))
      (funcall (plist-get (car auth-info) :secret))
    (error "DeepL API key not found in auth-source")))

(defun deepl--resolve-key (key)
  "If the given KEY is a function call it and return the result, otherwise
return KEY."
  (cond ((functionp key)                                (funcall key))
        ((and (stringp key) (not (string-empty-p key))) key)
        (t  (user-error "[INFO] Invalid API key, please set it to the correct value: %s" key))))

(defun deepl--alist-omit-null (alist)
  "Omit null value or empty string in ALIST."
  (cl-remove-if (lambda (pair)
                  (let ((value (cdr pair)))
                    (or (null value)          ; ignore null
                        (and (stringp value)  ; ignore empty string
                             (string-empty-p value)))))
                alist))

(defun deepl--headers (key)
  "Construct request headers.

Argument KEY is common request headers."
  (setq key (deepl--resolve-key key))
  (deepl--alist-omit-null `(("Authorization" . ,(if (or (null key)
                                                        (string-empty-p key))
                                                    ""
                                                  (concat "DeepL-Auth-Key " key))))))

(defun deepl--handle-error (response)
  "Handle error status code from the RESPONSE."
  (let ((status-code (request-response-status-code response)))
    (deepl--log "[ERROR]: %s" response)
    (pcase status-code
      (400 (message "400 - Bad request.  Please check error message and your parameters"))
      (401 (message "401 - Invalid Authentication"))
      (429 (message "429 - Rate limit reached for requests"))
      (500 (message "500 - The server had an error while processing your request"))
      (_   (message "Internal error: %s" status-code)))))

(defvar deepl-error nil
  "Records for the last error.")

(defmacro deepl-request (url &rest body)
  "Wrapper for `request' function.

The URL is the url for `request' function; then BODY is the arguments for rest."
  (declare (indent 1))
  `(progn
     (setq deepl-error nil)
     (request ,url
       :error (cl-function
               (lambda (&key response &allow-other-keys)
                 (setq deepl-error response)
                 (deepl--handle-error response)))
       ,@body)))

;;
;;; API

;;;###autoload
(cl-defun deepl-translate ( text target-lang callback
                            &key
                            (base-url deepl-base-url)
                            (parameters deepl-parameters)
                            (key deepl-key))
  "Send translate request.

Arguments MESSAGES and CALLBACK are required for this type of request.
TEXT is the text to translate.  TARGET-LANG is the target language to be
translated.  CALLBACK is the execuation after request is made.

Arguments BASE-URL, PARAMETERS and KEY are global options; however, you can
overwrite the value by passing it in."
  (deepl-request (concat base-url "/translate")
    :type "POST"
    :params parameters
    :headers (deepl--headers key)
    :data `(("text"        . ,text)
            ("target_lang" . ,target-lang))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;
;;; Application

(defun deepl-translate-this ()
  "Input text for translation."
  (interactive)
  (let ((text (read-string "Text to be translated: "))
        (target-lang (read-string "Translate to what language? " "ZH")))
    (deepl-translate text target-lang
                     (lambda (data)
                       (message "%s" data)))))

(provide 'deepl)
;;; deepl.el ends here
