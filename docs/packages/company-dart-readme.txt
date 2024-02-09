Add something similar to your init file
(add-hook 'dart-mode-hook (lambda ()
   (set (make-local-variable 'company-backends)
     '(company-dart (company-dabbrev company-yasnippet)))))

Dart completion will be invoked only after the "." character has been typed.
You can manually invoke completion by binding (company-dart)
to any key you like. Hitting F1 while in the completion list will show the
documentation for that candidate.

A good source for snippets
https://github.com/JEG2/dotfiles/tree/master/emacs.d/jeg2/snippets/dart-mode/

https://github.com/expez/company-quickhelp. Shows complete documentation as
a popup.
