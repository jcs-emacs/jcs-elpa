;;; jcs-modeline.el --- A modeline for jcs-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-emacs/jcs-modeline
;; Package-Version: 20221116.1838
;; Package-Commit: a8003837bcf626e7a815362054951f66ee6f2d44
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (moody "0.7.1") (minions "0.3.7"))
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

(require 'moody)
(require 'minions)

(defgroup jcs-modeline nil
  "A modeline for jcs-emacs."
  :prefix "jcs-modeline-"
  :group 'faces
  :link '(url-link :tag "Github" "https://github.com/jcs-emacs/jcs-modeline"))

;;
;; (@* "Entry" )
;;

(defvar jcs-modeline--default-mode-line nil
  "Default modeline value to revert back.")

(defun jcs-modeline--enable ()
  "Enable function `jcs-modeline-mode'."
  (setq jcs-modeline--default-mode-line mode-line-format)
  (setq-default mode-line-format
                '((:eval
                   (jcs-modeline-render
                    (quote
                     ("%e "
                      mode-line-front-space
                      mode-line-buffer-identification " "
                      (:eval (moody-tab (concat " " (format-mode-line mode-line-modes))))
                      " " (:eval (jcs-modeline--vc-project))))
                    (quote
                     ((:eval
                       (when (and (bound-and-true-p flycheck-mode)
                                  (or flycheck-current-errors
                                      (eq 'running flycheck-last-status-change)))
                         (cl-loop for state in '((error   . "#FB4933")
                                                 (warning . "#FABD2F")
                                                 (info    . "#83A598"))
                                  as lighter = (jcs-modeline--flycheck-lighter (car state))
                                  when lighter
                                  concat (propertize lighter 'face `(:foreground ,(cdr state))))))
                      (:eval (jcs-modeline--vc-info)) " "
                      (:eval (moody-tab " %l : %c " 0 'up)) " %p "
                      mode-line-end-spaces))))))
  (minions-mode 1))

(defun jcs-modeline--disable ()
  "Disable function `jcs-modeline-mode'."
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

(defun jcs-modeline--light-theme-p ()
  "Return non-nil if current theme is light theme."
  (ignore-errors (jcs-light-color-p (face-background 'default))))

;;
;; (@* "Core" )
;;

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
  (let* ((len-left (length (format-mode-line left)))
         (len-right (length (format-mode-line right)))
         (available-width (- (window-width) (+ len-left len-right)))
         (available-width (+ available-width (jcs-modeline--adjust-pad))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

;;
;; (@* "Plugins" )
;;

(defun jcs-modeline--vc-info ()
  "Return vc-mode information."
  (format-mode-line '(vc-mode vc-mode)))

(defun jcs-modeline--vc-project ()
  "Return the project name."
  (when-let ((project (jcs-project-root)))
    (file-name-nondirectory (directory-file-name project))))

(defun jcs-modeline--flycheck-lighter (state)
  "Return flycheck information for the given error type STATE."
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "?"))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running) (format "•%s" err))))

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
