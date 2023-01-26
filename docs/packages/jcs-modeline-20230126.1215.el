;;; jcs-modeline.el --- A modeline for jcs-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-emacs/jcs-modeline
;; Package-Version: 20230126.1215
;; Package-Commit: 1f0840dfeb7d459d8fc9109579fa1cadb9579f6e
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (moody "0.7.1") (minions "0.3.7") (elenv "0.1.0"))
;; Keywords: faces mode-line

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
;; A modeline for jcs-emacs
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'moody)
(require 'minions)
(require 'elenv)

(defgroup jcs-modeline nil
  "A modeline for jcs-emacs."
  :prefix "jcs-modeline-"
  :group 'faces
  :link '(url-link :tag "Github" "https://github.com/jcs-emacs/jcs-modeline"))

(defcustom jcs-modeline-left
  `("%e "
    mode-line-front-space
    (:eval (jcs-modeline--render-buffer-identification))
    (:eval (jcs-modeline--render-modes))
    (:eval (jcs-modeline--render-vc-project))
    (:eval (jcs-modeline--render-undo-tree-buffer-name)))
  "List of item to render on the left."
  :type 'list
  :group 'jcs-modeline)

(defcustom jcs-modeline-right
  `((:eval (jcs-modeline--render-nov))
    (:eval (jcs-modeline--render-text-scale))
    (:eval (jcs-modeline--render-flymake))
    (:eval (jcs-modeline--render-flycheck))
    (:eval (jcs-modeline--render-vc-info))
    (:eval (jcs-modeline--render-line-columns))
    " %p"
    mode-line-end-spaces)
  "List of item to render on the right."
  :type 'list
  :group 'jcs-modeline)

(defcustom jcs-modeline-checker-colors '((error   . "#FB4933")
                                         (warning . "#FABD2F")
                                         (info    . "#83A598")
                                         (note    . "#83A598"))
  "Alist of colors for checkers."
  :type 'list
  :group 'jcs-modeline)

;;
;; (@* "Externals" )
;;

(declare-function string-pixel-width "subr-x.el")   ; TODO: remove this after 29.1
(declare-function shr-string-pixel-width "shr.el")  ; TODO: remove this after 29.1

(defvar undo-tree-visualizer-buffer-name)
(defvar undo-tree-visualizer-parent-buffer)

(defvar flymake--state)
(declare-function flymake-running-backends "ext:flymake.el")
(declare-function flymake-disabled-backends "ext:flymake.el")
(declare-function flymake-reporting-backends "ext:flymake.el")
(declare-function flymake--diag-type "ext:flymake.el")
(declare-function flymake--state-diags "ext:flymake.el")

(defvar flycheck-current-errors)
(defvar flycheck-last-status-change)
(declare-function flycheck-has-current-errors-p "ext:flycheck.el")
(declare-function flycheck-count-errors "ext:flycheck.el")

;;
;; (@* "Entry" )
;;

(defvar jcs-modeline--render-left nil)
(defvar jcs-modeline--render-right nil)

(defvar jcs-modeline--default-mode-line nil
  "Default modeline value to revert back.")

(defun jcs-modeline--enable ()
  "Enable function `jcs-modeline-mode'."
  (unless elenv-graphic-p
    (advice-add 'moody-tab :override #'jcs-modeline--moody-tab))
  (add-hook 'window-size-change-functions #'jcs-modeline--window-resize)
  (jcs-modeline--window-resize)  ; call it manually once
  (setq jcs-modeline--default-mode-line mode-line-format)
  (setq-default mode-line-format
                '((:eval (jcs-modeline-render jcs-modeline--render-left
                                              jcs-modeline--render-right))))
  (minions-mode 1))

(defun jcs-modeline--disable ()
  "Disable function `jcs-modeline-mode'."
  (unless elenv-graphic-p
    (advice-remove 'moody-tab #'jcs-modeline--moody-tab))
  (remove-hook 'window-size-change-functions #'jcs-modeline--window-resize)
  (setq-default mode-line-format jcs-modeline--default-mode-line))

;;;###autoload
(define-minor-mode jcs-modeline-mode
  "Minor mode `jcs-modeline-mode'."
  :global t
  :require 'jcs-modeline-mode
  :group 'jcs-modeline
  :lighter nil
  (if jcs-modeline-mode (jcs-modeline--enable) (jcs-modeline--disable)))

;;
;; (@* "Util" )
;;

;; TODO: Use function `string-pixel-width' after 29.1
(defun jcs-modeline--string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp #'string-pixel-width)
      (string-pixel-width str)
    (require 'shr)
    (shr-string-pixel-width str)))

(defun jcs-modeline--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (jcs-modeline--string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun jcs-modeline--light-color-p (hex-code)
  "Return non-nil if HEX-CODE is in light tone."
  (when elenv-graphic-p
    (let ((gray (nth 0 (color-values "gray")))
          (color (nth 0 (color-values hex-code))))
      (< gray color))))

(defun jcs-modeline--light-theme-p ()
  "Return non-nil if current theme is light theme."
  (ignore-errors (jcs-modeline--light-color-p (face-background 'default))))

(defun jcs-modeline-format (format &optional face window buffer)
  "Wrapper for function `format-mode-line'."
  (string-trim (format-mode-line format face window buffer)))

;;
;; (@* "Core" )
;;

(defun jcs-modeline--window-resize (&rest _)
  "Window resize hook."
  (let ((count 0) (current-width 0)
        (is-left t)
        (left-index 0) (right-index 0)
        ;; Let's iterate it from outer to inner, so we must flip the right list.
        (right-list (reverse jcs-modeline-right)))
    (setq jcs-modeline--render-left nil
          jcs-modeline--render-right nil)  ; reset
    ;; Loop through the entire `left' + `right' list
    (while (< count (length (append jcs-modeline-left jcs-modeline-right)))
      ;; Check if the index exceed it's length, then flip the left/right
      ;; toggle variable `is-left' to ensure we iterate through the whole
      ;; render list!
      (when (<= (length (if is-left jcs-modeline-left jcs-modeline-right))
                (if is-left left-index right-index))
        (setq is-left (not is-left)))
      ;; Select the item, check the length and add it to the render list!
      (let* ((item (nth (if is-left left-index right-index)
                        (if is-left jcs-modeline-left right-list)))
             (format (format-mode-line item))
             (width (jcs-modeline--str-len format))
             (new-width (+ current-width width)))
        ;; Can the new item added to the list?
        (when (<= new-width (window-width))  ; can be displayed properly!
          (setq current-width new-width)  ; update string/display width
          ;; Add the item to render list!
          (push item (if is-left jcs-modeline--render-left
                       jcs-modeline--render-right))))
      (cl-incf (if is-left left-index right-index))  ; increment index
      (cl-incf count)
      (setq is-left (not is-left))))  ; flip `left' and `right'
  (setq jcs-modeline--render-left (reverse jcs-modeline--render-left)
        ;; Since we iterate it from the edge, we don't need to reverse the right
        jcs-modeline--render-right jcs-modeline--render-right))

(defun jcs-modeline--adjust-pad ()
  "Adjust padding for external packages."
  (let ((delta 0))
    (when vertical-scroll-bar
      (when-let* ((data (window-scroll-bars))
                  (shown (nth 2 data))
                  (width (nth 1 data)))
        (setq delta (+ delta width))))
    delta))

(defun jcs-modeline-render (left right)
  "Render mode line with LEFT and RIGHT alignment."
  (let* ((len-left (jcs-modeline--str-len (format-mode-line left)))
         (len-right (jcs-modeline--str-len (format-mode-line right)))
         (available-width (- (window-width) (+ len-left len-right)))
         (available-width (+ available-width (jcs-modeline--adjust-pad))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(defun jcs-modeline--moody-tab (arg0 &rest _)
  "Override `moody-ta' function when inside the terminal."
  (concat " " arg0 " "))

;;
;; (@* "Plugins" )
;;

;;
;;; Buffe Identification

(defun jcs-modeline--render-buffer-identification ()
  "Render buffer identification."
  (concat (jcs-modeline-format mode-line-buffer-identification) " "))

;;
;;; Modes

(defun jcs-modeline--render-modes ()
  "Render line modes."
  (let ((line-modes (jcs-modeline-format (if minions-mode
                                             minions-mode-line-modes
                                           mode-line-modes))))
    (moody-tab line-modes)))

;;
;;; Line and Columns

(defun jcs-modeline--render-line-columns ()
  "Render current line number and column."
  (moody-tab "%l : %c" 0 'up))

;;
;;; Project

(defun jcs-modeline--render-vc-info ()
  "Return `vc-mode' information."
  (when-let ((info (jcs-modeline-format '(vc-mode vc-mode))))
    (unless (string-empty-p info) (concat info " "))))

(defun jcs-modeline--project-root ()
  "Return project directory path."
  (when-let ((current (project-current))) (project-root current)))

(defcustom jcs-modeline-show-project-name-virutal-buffer nil
  "If non-nil, display project-name in virutal buffer."
  :type 'boolean
  :group 'jcs-modeline)

(defun jcs-modeline--render-vc-project ()
  "Return the project name."
  (when (or (buffer-file-name) jcs-modeline-show-project-name-virutal-buffer)
    (when-let ((project (jcs-modeline--project-root)))
      (concat " " (file-name-nondirectory (directory-file-name project))))))

;;
;;; Text Scale

(defun jcs-modeline--render-text-scale ()
  "Render text-scale amount."
  (when (and (boundp 'text-scale-mode-amount) (/= text-scale-mode-amount 0))
    (format
     (if (> text-scale-mode-amount 0)
         "(%+d) "
       "(%-d) ")
     text-scale-mode-amount)))

;;
;;; Undo

(defun jcs-modeline--render-undo-tree-buffer-name ()
  "Render text-scale amount."
  (when (featurep 'undo-tree)
    (cond ((equal (buffer-name) undo-tree-visualizer-buffer-name)
           (format " %s" undo-tree-visualizer-parent-buffer)))))

;;
;;; Flymake

(defun jcs-modeline--flymake-lighter (diags-by-type state running)
  "Return flycheck lighter by given STATE.

If argument RUNNING is non-nil, we turn lighter into question mark."
  (let* ((c-state (cl-case state
                    (error   :error)
                    (warning :warning)
                    (note    :note)))
         (counts (length (gethash c-state diags-by-type)))
         (color (cdr (assoc state jcs-modeline-checker-colors)))
         (lighter (format "%s" (if running "?" counts))))
    (propertize lighter 'face `(:foreground ,color))))

(defun jcs-modeline--render-flymake ()
  "Render for flymake."
  (when (bound-and-true-p flymake-mode)
    (let* ((known (hash-table-keys flymake--state))
           (running (flymake-running-backends))
           (disabled (flymake-disabled-backends))
           (reported (flymake-reporting-backends))
           (diags-by-type (make-hash-table))
           (all-disabled (and disabled (null running)))
           (some-waiting (cl-set-difference running reported)))
      (maphash (lambda (_b state)
                 (mapc (lambda (diag)
                         (push diag
                               (gethash (flymake--diag-type diag)
                                        diags-by-type)))
                       (flymake--state-diags state)))
               flymake--state)
      (concat
       (let* ((states '(error warning note))
              (last (car (last states)))
              result)
         (dolist (state states)
           (when-let ((lighter (jcs-modeline--flymake-lighter
                                diags-by-type state
                                (or some-waiting (null known) all-disabled))))
             (setq result (concat result lighter
                                  (unless (equal state last) "/")))))
         result)
       " "))))

;;
;;; Flycheck

(defun jcs-modeline--flycheck-lighter (state running)
  "Return flycheck lighter by given STATE.

If argument RUNNING is non-nil, we turn lighter into question mark."
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (err (or (cdr (assq state counts)) "0"))
         (color (cdr (assoc state jcs-modeline-checker-colors)))
         (lighter (format "%s" (if running "?" err))))
    (propertize lighter 'face `(:foreground ,color))))

(defun jcs-modeline--render-flycheck ()
  "Render for flycheck."
  (when (bound-and-true-p flycheck-mode)
    (concat
     (let* ((states '(error warning info))
            (last (car (last states)))
            (running (eq 'running flycheck-last-status-change))
            result)
       (dolist (state states)
         (when-let ((lighter (jcs-modeline--flycheck-lighter state running)))
           (setq result (concat result lighter
                                (unless (equal state last) "/")))))
       result)
     " ")))

;;
;;; Nov

(defvar nov-documents)
(defvar nov-documents-index)

(defun jcs-modeline--render-nov ()
  "Render for nov."
  (when (eq major-mode 'nov-mode)
    (format "[%s/%s]" (1+ nov-documents-index) (length nov-documents))))

;;
;; (@* "Themes" )
;;

(defun jcs-modeline--set-color (ac-lst inac-lst)
  "Set `mode-line' theme faces with AC-LST and INAC-LST."
  (let ((state (frame-focus-state))
        (ac-0 (nth 0 ac-lst)) (ac-1 (nth 1 ac-lst))
        (ic-0 (nth 0 inac-lst)) (ic-1 (nth 1 inac-lst)))
    (set-face-foreground 'mode-line (if state ac-0 ic-0))
    (set-face-background 'mode-line (if state ac-1 ic-1))
    (set-face-foreground 'mode-line-inactive ic-0)
    (set-face-background 'mode-line-inactive ic-1)))

(defun jcs-modeline--set-border-color (color)
  "Set the border color with COLOR."
  (set-face-foreground 'vertical-border color)
  (set-face-foreground 'window-divider color))

(defun jcs-modeline--set-theme (ml-ac-lst ml-inac-lst bc)
  "Set the mode line theme.
ML-AC-LST : mode-line active list.  ML-INAC-LST : mode-line inactive list.
BC : border color."
  (jcs-modeline--set-color ml-ac-lst ml-inac-lst)
  (jcs-modeline--set-border-color bc))

(defun jcs-modeline-gray ()
  "Gray mode line."
  (interactive)
  (if (jcs-modeline--light-theme-p)
      (jcs-modeline--set-theme
       '("#1C1C1C" "#E5E5E5") '("#000000" "#D7D7D7") "#161616")
    (jcs-modeline--set-theme
     '("#D2D2D2" "#4D4D4D") '("#CCCCCC" "#333333") "#D2D2D2")))

(defun jcs-modeline-dark-green ()
  "Dark green mode line."
  (interactive)
  (if (jcs-modeline--light-theme-p)
      (jcs-modeline--set-theme
       '("#000" "#B7D3D3") '("#000" "#99C2C2") "#B7D3D3")
    (jcs-modeline--set-theme
     '("#CCCCCC" "#3C6A69") '("#CCCCCC" "#2B4D4D") "#3C6A69")))

(defun jcs-modeline-dark-blue ()
  "Dark blue mode line."
  (interactive)
  (if (jcs-modeline--light-theme-p)
      (jcs-modeline--set-theme
       '("#000" "#92B9DF") '("#000" "#7AA0C6") "#92B9DF")
    (jcs-modeline--set-theme
     '("#CCCCCC" "#205386") '("#CCCCCC" "#0C3765") "#246AAF")))

(defun jcs-modeline-dark-orange ()
  "Dark orange mode line."
  (interactive)
  (if (jcs-modeline--light-theme-p)
      (jcs-modeline--set-theme
       '("#1C1C1C" "#CC6633") '("#000000" "#682B12") "#CC6633")
    (jcs-modeline--set-theme
     '("#D2D2D2" "#CC6633") '("#CCCCCC" "#A4532A") "#CC6633")))

(defun jcs-modeline-red ()
  "Red mode line."
  (interactive)
  (if (jcs-modeline--light-theme-p)
      (jcs-modeline--set-theme
       '("#CCCCCC" "#FF0000") '("#CCCCCC" "#6A0101") "#FF0000")
    (jcs-modeline--set-theme
     '("#CCCCCC" "#FF0000") '("#CCCCCC" "#6A0101") "#FF0000")))

(defun jcs-modeline-purple ()
  "Purple mode line."
  (interactive)
  (if (jcs-modeline--light-theme-p)
      (jcs-modeline--set-theme
       '("#CCCCCC" "#B100EB") '("#CCCCCC" "#650286") "#B100EB")
    (jcs-modeline--set-theme
     '("#CCCCCC" "#B100EB") '("#CCCCCC" "#650286") "#B100EB")))

(provide 'jcs-modeline)
;;; jcs-modeline.el ends here
