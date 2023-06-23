;;; sound-async.el --- Play sound asynchronously  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/sound-async
;; Package-Version: 20221019.1127
;; Package-Commit: 324c07ad4624d6b31ba0a077c71ee2a56d0c392d
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (async "1.9.3"))
;; Keywords: multimedia

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
;; Play sound asynchronously
;;

;;; Code:

(require 'async)

(defgroup sound-async nil
  "Play sound asynchronously."
  :prefix "sound-async-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/sound-async"))

;;;###autoload
(defun sound-async-play (file volume &optional callback)
  "Play sound FILE asynchronously.

See function `play-sound-file' for argument VOLUME.  The CALLBACK is called
after the sound is done playing."
  (async-start
   (lambda (&rest _) (play-sound-file file volume))
   (lambda (&rest _) (funcall callback file volume))))

;;;###autoload
(defun sound-async-stop (process)
  "Stop the sound PROCESS."
  (when (processp process)
    (ignore-errors (kill-process process))
    (ignore-errors (kill-buffer (process-buffer process)))))

(provide 'sound-async)
;;; sound-async.el ends here
