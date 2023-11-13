A little tool which may help to bump version for your projects files.

; Installation

Assuming that you cloned emacs-bump-version to the
`~/.emacs.d/bump-version/' folder. Add the following lines to your
`.emacs' file:

(add-to-list 'load-path "~/.emacs.d/bump-version/")
(require 'bump-version)
(global-set-key (kbd "C-c C-b p") 'bump-version-patch)
(global-set-key (kbd "C-c C-b i") 'bump-version-minor)
(global-set-key (kbd "C-c C-b m") 'bump-version-major)


; Configuration


You can create config file for your project by means of
`bump-version-create-config' command. It just creates `.bump-version.el' file in
the `default-directory'. `bump-version-create-config` command creates and opens
`.bump-version.el` file. Just edit it (add file names in which project version
needs to be updated) and save.

Or you can create it manually. Add `.bump-version.el' file to your project root
directory. For example, for `bump-version' project it looks like the following:

((:files
  ("Cask"
   "bump-version.el"
   "bump-version-pkg.el"))
 (:current-version "0.2.1"))

Then you can interactively call `bump-version-patch',
`bump-version-minor' or `bump-version-major' commands and a version
will be updated for the specified files (also for the
`.bump-version.el' file).

; Contribution

Contribution is much welcome! When adding new features, please write tests for them!

Thank you! And Enjoy!
