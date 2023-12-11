
Emacs major mode for NAND hardware description language files (.hdl) from
the coursera class nand2tetris.

The mode provides:

- syntax / font-locking
- customizable indentation: choose indentation for IN/OUT declarations,
  PARTS, and general offset.
- Compilation support, running the chip in simulator, and jumping to mismatches
  in .out file.
- Comparison buffer for expected vs. output. (`C-c C-c`)
- imenu, other user functions, etc.
- Autocompletion / dropdown help using `company-nand' with
  `company-mode' and `company-quickhelp'
- snippets to fill in chip components

Tools in build directory:
- Autogenerate snippets from the HDL survival website (for use with yasnippet)
- Autogenerate docs.txt from builtin chips.

Todo:
- Completion-at-point functions.
- Jump to definitions
