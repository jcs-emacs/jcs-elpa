Note: for original author's details see:
[VbsReplMode](http://www.emacswiki.org/emacs/VbsReplMode)

Separated inferior repl from major mode, added company keyword support.

Usage:

To use this, put it in your load-path and do something like the
following in your .emacs file:

   ; VBScript editing
   (setq auto-mode-alist
      (append '(("\\.\\(vbs\\|wsf\\)$" . vbscript-mode))
              auto-mode-alist))

To start the repl, load a VBScript file, make sure you're in
visual-basic mode, and hit C-c C-z (or whatever you mapped to
vbs-setup-repl).  To execute from the beginning of the line (or the
beginning of the statement; it does recognize multiline VBScript),
use C-c C-e.  Select a region and execute it with C-c C-r.  If you
want it to Eval rather than Execute (like, for example, you want to
know what "x & y" would be), Use C-u before the above keystrokes.
