To use this package, simply install and add this to your init.el
(require 'nerd-icons-archive)
(add-hook 'tar-mode-hook 'nerd-icons-archive-mode)

or use use-package:
(use-package nerd-icons-archive
  :hook
  (tar-mode . nerd-icons-archive-mode))

This package is inspired by
- `nerd-icons-dired': https://github.com/rainstormstudio/nerd-icons-dired
