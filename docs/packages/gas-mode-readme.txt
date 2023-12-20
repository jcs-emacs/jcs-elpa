Provides font-locking and indentation support for Assembly code with AT&T
syntax, usually code written for GAS (Gnu Assembler).

If you are installing manually, you should add the following code to your
.emacs file:
     (require 'gas-mode)
     (add-to-list 'auto-mode-alist '("\\.asm\\'" . gas-mode))

Currently works with basic AT&T syntax, having basic font-locking and
indentation.

TODO list of improvements or features:

  - Improve indentation command to indent tags and .section to either
    the beginning of the line (depends on the value of `gas-initial-indent')
    or to the normal indentation (`gas-indentation') on the second time the
    command is executed.

  - Add support for company.

  - Add support for imenu.
