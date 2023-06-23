;;; zoom-to-point.el --- Zoom in or out on the line at point.

;; Copyright (C) 2012, 2013 Rich Loveland

;; Author: Rich Loveland  <r@rmloveland.com>
;; Keywords: convenience, text size, zoom
;; Package-Version: 20131216.1307
;; Package-Commit: 38b11fe8f110eae4c8c7e1a0b40e05f9e58c7f36
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Exposes a command, `zoom-toggle', which will increase or decrease
;;  the size of the text at point, and a variable,
;;  `*default-zoom-level*', which determines the change in size.

;;; Code:

(defvar *default-zoom-level* 3
  "Defines how much the `zoom-toggle' command will increase the text scale.")

;; This variable is buffer-local, and isn't always initialized; this
;; causes an error.  We initialize it here to avoid causing errors.
;; So far it's worked well.
(setq text-scale-mode-amount 0)

(defun zoom-toggle ()
  "Zoom in or out on the line at point.
More precisely, increase or decrease the size of the text by the
amount defined in `*default-zoom-level*', and centers that line
in the window."
  (interactive)
  (if (zoom--zoomed-p)
      (zoom--unzoom)
    (zoom--zoom-to-point)))

(defun zoom--do (thunk)
  "Call THUNK, and then recenter point."
  (save-excursion
    (funcall thunk)
    (recenter)))

(defun zoom--zoom-to-point ()
  "Zoom in on point."
  (zoom--do (lambda ()
	     (text-scale-increase *default-zoom-level*))))

(defun zoom--unzoom ()
  "Zoom out from point and return to the default text scale."
  (zoom--do (lambda ()
    (text-scale-set 0))))

(defun zoom--zoomed-p ()
  "Is the buffer text larger than the default?"
  (if (= text-scale-mode-amount 0)
      nil
    t))

(provide 'zoom-to-point)

;;; zoom-to-point.el ends here
