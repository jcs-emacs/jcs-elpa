Major mode for editing HLSL grammar files, usually files ending
with '(.fx|.hlsl)'.  Is is based on c-mode plus some features
and pre-specified fontifications.

; Installation:

This file requires Emacs-20.3 or higher and package cc-mode.

If hlsl-mode is not part of your distribution, put this file into your
load-path and the following into your ~/.emacs:
  (autoload 'hlsl-mode "hlsl-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.fx\\'" . hlsl-mode))
