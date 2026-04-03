This package integrates janet with Emacs via flycheck.  To use it,
add to your .emacs-equivalent (e.g. init.el):

  (require 'flycheck-janet)

likely something like:

  (global-flycheck-mode)

will be necessary too.

Make sure the janet binary is on your path.
