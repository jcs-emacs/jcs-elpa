
Visit https://github.com/elp-revive/kaomoji.el
to see screenshot & usage guide.

======= Usage =======
(require 'kaomoji), then M-x `kaomoji'

=== Customization ===
Variables:

`kaomoji-table' : The main table contains '(((ALIAS ...) . KAOMOJI) ...)
You can customize like this to append new items to this talbe:

(setq kaomoji-table
      (append '((("angry" "furious") . "(／‵Д′)／~ ╧╧ ")
                (("angry" "punch") . "#ﾟÅﾟ）⊂彡☆))ﾟДﾟ)･∵"))
              kaomoji-table))

`kaomoji-insert-user-input-at' : 'left-side or 'right-side
