;;; quick-peek.el --- Inline quick-peek windows      -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2018  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; Keywords: tools help doc convenience
;; Package-Version: 20220223.1849
;; Package-Commit: 746c75110e3e149dbeeb9e4d39d9465d141708ed
;; Package-Requires: ((emacs "24.3"))
;; Version: 1.0

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

;; A utility library to display inline pop-ups.  Looks roughly like this:
;;
;;
;; let _ = <|>le m n                           ← <|> marks the point
;; ------------------------------------------- ← Pop-up begins here
;;         le : ℕ → ℕ → ℙ
;;         Inductive le (n : ℕ) : ℕ → ℙ ≜
;;         | le_n : n ≤ n
;;         | le_S : ∀ m : ℕ, n ≤ m → n ≤ S m
;; ------------------------------------------- ← Pop-up ends here
;;         && le n m                           ← Buffer text continues here
;;
;; See `quick-peek-show' and `quick-peek-hide' for usage instructions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup quick-peek nil
  "Customization group for the `quick-peek' package."
  :group 'help
  :group 'tools
  :tag "Quick peek windows")

(defcustom quick-peek-position 'below
  "Position of quick-peek windows."
  :group 'quick-peek
  :type '(choice (const :tag "Above the current line" above)
                 (const :tag "Below the current line" below)))

(defcustom quick-peek-add-spacer t
  "If non-nil, display spacer above and below quick-peek contents."
  :group 'quick-peek
  :type 'boolean)

;;; Variables

(defvar-local quick-peek--overlays nil
  "Overlays currently showing quick peek windows.")

;;; Faces

(defface quick-peek-background-face
  '((t :inherit default :extend t))
  "Face added to all text in quick-peek windows."
  :group 'quick-peek)

(defface quick-peek-border-face
  '((t :height 1 :extend t))
  "Face added to quick-peek window borders."
  :group 'quick-peek)

(defface quick-peek-padding-face
  '((t :height 0.15 :extend t))
  "Face added to quick-peek window padding."
  :group 'quick-peek)

;;; Utilities

(defun quick-peek--point-at-bovl ()
  "Return point that `beginning-of-visual-line' would jump to."
  (save-excursion
    (beginning-of-visual-line)
    (point)))

(defun quick-peek--count-visible-lines-under (pos)
  "Count number of lines visible in selected window under POS."
  (save-excursion
    (goto-char pos)
    (let ((line-move-visual 1)
          (win-start (window-start nil))
          (available-lines (window-body-height)))
      (while (> (point) win-start)
        (vertical-motion -1)
        (cl-decf available-lines))
      available-lines)))

(defun quick-peek--text-width (from to)
  "Measure the width of the text between FROM and TO.
Results are meaningful only if FROM and TO are on the same line."
  ;; `current-column' takes prettification into account
  (- (save-excursion (goto-char to) (current-column))
     (save-excursion (goto-char from) (current-column))))

(defun quick-peek--max-line-length ()
  "Return the max line width in columns in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((maxlen 0))
      (while (not (eobp))
        (let ((line-len (quick-peek--text-width (point-at-bol) (point-at-eol))))
          (setq maxlen (max line-len maxlen)))
        (forward-line 1))
      maxlen)))

(defun quick-peek--truncate-buffer (start n-lines &optional ellipsis)
  "Truncate current buffer N-LINES after START.
Optionally adds an ELLIPSIS at the end."
  (cl-assert (and n-lines (> n-lines 0)))
  (save-excursion
    (goto-char start)
    (forward-line n-lines)
    (unless (eobp)
      (delete-region (point) (point-max))
      (forward-line -1)
      (goto-char (point-at-eol))
      (insert (or ellipsis " …")))))

(defun quick-peek--prefix-all-lines (prefix)
  "Add a PREFIX to all lines of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (insert prefix)
      (forward-line 1))))

(defun quick-peek--last-visible-pos-in-window ()
  "Last point in current visible window."
  (save-excursion
    (ignore-errors (move-to-window-line -1))
    (line-beginning-position)))

(defun quick-peek--last-line-in-buffer-p ()
  "Is current line the last line in buffer."
  (= (line-number-at-pos (point) t) (line-number-at-pos (point-max) t)))

(defun quick-peek--last-visible-line-in-window ()
  "Last line number in current visible window."
  (line-number-at-pos (quick-peek--last-visible-pos-in-window) t))

(defun quick-peek--form-face (fg &optional weight)
  "Form `quick-peek' face with FG."
  (let ((weight (or weight 'normal))
        (color (or (face-attribute 'highlight :background) "black")))
    `(:background ,color :foreground ,fg :inherit quick-peek-border-face :weight ,weight)))

(defun quick-peek--fill-n-char-seq (ch-seq n)
  "Fill CH-SEQ with N length."
  (when-let ((ch-out ch-seq) (n (or n 1)))
    (while (< (length ch-out) n)
      (setq ch-out (concat ch-out ch-seq)))
    (when ch-out (substring ch-out 0 n))))

(defvar quick-peek--spacer-header nil "Header string for `quick-peek'")
(defvar quick-peek--spacer-footer nil "Footer string for `quick-peek'.")

(defun quick-peek-set-spacers (buf)
  "Prepare quick peek header and footer."
  (let ((default-face (quick-peek--form-face "white")))
    (setq quick-peek--spacer-header
          (concat
           (if (quick-peek--last-line-in-buffer-p) "\n" "")
           (propertize " " 'face default-face)
           (propertize (buffer-name buf) 'face (quick-peek--form-face "black" 'bold))
           (propertize " " 'face default-face)
           (propertize (buffer-file-name buf) 'face (quick-peek--form-face "#222"))
           (propertize "\n" 'face default-face))
          quick-peek--spacer-footer
          (propertize (concat
                       "\n"
                       (if (eq quick-peek-position 'below) "" "\n"))
                      'face default-face))))

(defun quick-peek--scroll-to-see ()
  "Scroll buffer in order to see the full `quick-peek' content."
  (let ((default-max-h 16) (ln-current (line-number-at-pos)) lvl ln-diff)
    (when (eq quick-peek-position 'below)
      (setq lvl (quick-peek--last-visible-line-in-window)
            ln-diff (- lvl ln-current))
      (when (< ln-diff default-max-h)
        (scroll-up-line (- default-max-h ln-diff))))))

(defun quick-peek--insert-spacer (pos _str-before _str-after)
  "Insert a thin horizontal line at POS.
Line is surrounded by STR-BEFORE and STR-AFTER."
  (let ((str (if (= pos (point-min)) quick-peek--spacer-header
               quick-peek--spacer-footer)))
    (save-excursion (goto-char pos) (insert str))))

;;; Core

(defun quick-peek--prepare-overlay-string (str offset &optional max-lines)
  "Prepare STR for display in a quick peek window.
Return value is a string that includes properties surrounding it
with two thin horizontal lines, indented by OFFSET, and truncated
to show at most MAX-LINES.  If text doesn't fit horizontally
between OFFSET and the end of the window, it will be moved left."
  (with-temp-buffer
    (insert str "\n")
    (when max-lines
      (quick-peek--truncate-buffer (point-min) max-lines " …"))
    (let* ((text-width (+ (quick-peek--max-line-length) 5)) ;; +5 for wide characters
           (max-offset (- (window-body-width) text-width))
           (real-offset (max 0 (min offset max-offset))))
      (quick-peek--prefix-all-lines (make-string real-offset ?\s)))
    (let ((char-property-alias-alist '((face font-lock-face))))
      (font-lock-append-text-property (point-min) (point-max) 'face 'quick-peek-background-face))
    (when quick-peek-add-spacer
      (quick-peek--insert-spacer (point-min) "\n" "\n")
      (quick-peek--insert-spacer (point-max) "\n" "\n"))
    (buffer-string)))

(defun quick-peek-overlay-contents (ov)
  "Return the raw string contents of quick-peek overlay OV.

Use `(setf (quick-peek-overlay-contents ov) str)' to change an
overlay's contents.  Setting an overlay's contents doesn't
automatically update it; you must call `quick-peek-update' for
that.

Most programs should not need to set this property directly, as
it is only useful if you intend to build quick-peek windows
piecemeal (most programs should call `quick-peek-show' directly
instead)."
  (overlay-get ov 'quick-peek--contents))

(gv-define-setter quick-peek-overlay-contents (str ov)
  `(overlay-put ,ov 'quick-peek--contents ,str))

(defun quick-peek--compute-height (_ov min-h max-h)
  "Compute an appropriate height for OV on the current line.

MIN-H and MAX-H are bounds on the height.  If MAX-H is `none',
let the inline window expand beyond the end of the selected Emacs
window."
  (unless (eq max-h 'none) ;; FIXME this should check `quick-peek-position'
    (let ((visible-lines (quick-peek--count-visible-lines-under (point))))
      (max min-h (min max-h (- visible-lines 2))))))

(defun quick-peek-update (ov &optional min-h max-h)
  "Redraw quick-peek overlay OV.

MIN-H and MAX-H are bounds on the height of the window.  If MAX-H
is `none', let the inline window expand beyond the end of the
selected Emacs window.

Calling this method is only useful after directly setting the
contents of a quick-peek overlay.  Most programs should not need
to do so (call `quick-peek-show' directly instead)."
  (save-excursion
    (goto-char (overlay-start ov))
    (setq min-h (or min-h 4) max-h (or max-h 16))
    (let* ((contents (quick-peek-overlay-contents ov))
           (offset (quick-peek--text-width (quick-peek--point-at-bovl) (point)))
           (height (quick-peek--compute-height ov min-h max-h))
           (str (quick-peek--prepare-overlay-string contents offset height))
           (prop (pcase quick-peek-position
                   (`above 'before-string)
                   (_ 'after-string))))
      (overlay-put ov prop str))))

(defun quick-peek-overlay-p (ov)
  "Check if OV is a quick-peek overlay."
  (overlay-get ov 'quick-peek))

;;;###autoload
(defun quick-peek-overlay-at (pos)
  "Find overlay for line at POS."
  (car (cl-remove-if-not (lambda (ov) (quick-peek--overlay-matches-pos ov pos)) quick-peek--overlays)))

(defun quick-peek-overlay-ensure-at (pos)
  "Find or create a quick-peek overlay for the line at POS.

Typical code should not need this method; use `quick-peek-show'
instead."
  (or (quick-peek-overlay-at pos)
      (let* ((ov (save-excursion
                   (goto-char pos)
                   (make-overlay (point-at-bol) (1+ (point-at-eol))))))
        (overlay-put ov 'quick-peek t)
        (push ov quick-peek--overlays)
        ov)))

(defun quick-peek--show-at (pos str &optional min-h max-h)
  "Show STR in inline window at POS.
MIN-H and MAX-H are bounds on the height of the window.  If MAX-H
is `none', let the inline window expand beyond the end of the
selected Emacs window."
  (let ((ov (quick-peek-overlay-ensure-at pos)))
    (setf (quick-peek-overlay-contents ov) str)
    (quick-peek-update ov min-h max-h)))

;;;###autoload
(defun quick-peek-show (str &optional pos min-h max-h)
  "Show STR in an inline window at POS.
MIN-H (default: 4) and MAX-H (default: 16) are bounds on the
height of the window.  Setting MAX-H to `none' allows the inline
window to expand past the bottom of the current Emacs window."
  (ignore (quick-peek--show-at (or pos (point)) str min-h max-h)))

(defun quick-peek--overlay-matches-pos (ov pos)
  "Check if OV is a quick-peek overlay covering POS."
  (and (quick-peek-overlay-p ov)
       (<= (overlay-start ov) pos)
       (< pos (overlay-end ov))))

;;;###autoload
(defun quick-peek-hide (&optional pos)
  "Hide inline windows.
With non-nil POS, clear only overlays on line of POS.
Return the number of overlays hidden."
  (interactive)
  (let ((kept nil)
        (nb-deleted 0))
    (dolist (ov quick-peek--overlays)
      (if (or (null pos) (quick-peek--overlay-matches-pos ov pos))
          (progn (delete-overlay ov)
                 (cl-incf nb-deleted))
        (push ov kept)))
    (setq quick-peek--overlays kept)
    nb-deleted))

(provide 'quick-peek)
;;; quick-peek.el ends here
