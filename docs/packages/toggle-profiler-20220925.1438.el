;;; toggle-profiler.el --- Useful functions to interact with profiler  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/toggle-profiler
;; Package-Version: 20220925.1438
;; Package-Commit: 81a9deb64384ad1d930e4e6f70be184f05728966
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience

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
;; Useful functions to interact with profiler.
;;

;;; Code:

(require 'profiler)

(defgroup toggle-profiler nil
  "Useful functions to interact with profiler."
  :prefix "toggle-profiler-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/toggle-profiler"))

(defvar toggle-profiler nil)

;;;###autoload
(defun toggle-profiler ()
  "Toggle the Emacs profiler. Run it again to see the profiling report."
  (interactive)
  (if (not toggle-profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq toggle-profiler (not toggle-profiler)))

(provide 'toggle-profiler)
;;; toggle-profiler.el ends here
