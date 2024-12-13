
   Show region information in the mode-line.

 More description below.


(@> "Index")

 Index
 -----

 If you have library `linkd.el' and Emacs 22 or later, load
 `linkd.el' and turn on `linkd-mode' now.  It lets you easily
 navigate around the sections of this doc.  Linkd mode will
 highlight this Index, as well as the cross-references and section
 headings throughout this file.  You can get `linkd.el' here:
 https://www.emacswiki.org/emacs/download/linkd.el.

 (@> "Things Defined Here")
 (@> "Documentation")
   (@> "Purpose and Getting Started")
   (@> "General Behavior")
   (@> "Controlling What Region Info To Show")
   (@> "Use Mode-Line Menus To Change Behavior")
   (@> "Highlight Large Column Numbers")
   (@> "Show That Additional Commands Are Regionable")

(@* "Things Defined Here")

 Things Defined Here
 -------------------

 Faces defined here:

   `mlr-column-warning', `mlr-region', `mlr-region-acting-on'.

 User options defined here:

   `mlr-column-limit', `mlr-count-partial-words-flag',
   `mlr-empty-region-flag', `mlr-init-menu-on-load-flag',
   `mlr-non-rectangle-style', `mlr-rectangle-style',
   `mlr-region-style', `mlr-use-property-mlr-acts-on-flag'.

 Commands defined here:

   `global-modeline-region-mode', `mlr-choose-region-style',
   `mlr-count-rectangle-contents', `modeline-region-mode',
   `mlr-toggle-non-rectangle-style', `mlr-toggle-rectangle-style',
   `mlr--advice-4', `mlr--advice-11', `mlr--advice-12',
   `mlr--advice-13', `mlr--advice-14', `mlr--advice-15',
   `mlr--advice-16', `mlr--advice-18', `mlr--advice-19',
   `mlr--advice-20', `mlr--advice-21'.

 Non-interactive functions defined here:

   `mlr--advice-1', `mlr--advice-2', `mlr--advice-3',
   `mlr--advice-10', `mlr--advice-17',
   `mlr-set-default-mode-line-position', `mlr-show-region-p',
   `turn-on-modeline-region-mode'.

 Constants defined here:

   `mlr--region-style-alist', `mlr--region-style-default'.

 Internal variables defined here:

   `mlr-bytes-format', `mlr-chars-format',
   `mlr-lines+chars-format', `mlr-lines+words+chars-format',
   `mlr--mlp-is-set-up-p', `mlr--orig-mlp-local-p',
   `mlr--orig-mode-line-position', `mlr-menu', `mlr-rect-p',
   `mlr-region-acting-on', `mlr-rows+cols-format',
   `mlr-rows+cols+words+chars-format', `modeline-region-mode-map'.

(@* "Documentation")

 Documentation
 -------------

(@* "Purpose and Getting Started")
 ** Purpose and Getting Started **

 Minor mode `modeline-region-mode' and its globalized version
 `global-modeline-region-mode' enhance the buffer size indication
 in the mode-line, that is, what is provided by vanilla option
 `mode-line-position' with `size-indication-mode' turned on.
 (These modes automatically turn on `size-indication-mode'.)

 Region information is shown in the mode-line when the region is
 active -- you can show the the region size in various ways.

 To use this library, put this file in a directory in your
 `load-path', and add this to your init file:

   (require 'modeline-region)

 If you want to turn on this showing of active-region information
 in the mode-line globally (in all buffers) then add this as well:

   (global-modeline-region-mode 1)

 Otherwise, to turn it on/off in a given buffer or globally, use
 command `modeline-region-mode' or `global-modeline-region-mode',
 respectively.

 (This library supersedes my older library `modeline-posn.el'.
 This one is more correct, more featureful, and cleaner.)

(@* "General Behavior")
 ** General Behavior **

 Whether an empty active region is indicated by these modes is
 controlled by option `mlr-empty-region-flag'.  By default an empty
 region is indicated, to avert you to the fact that you're acting
 on an empty region.

 When these modes are on, information about the region size is
 shown in the mode-line using face `mlr-region', by default.  By
 default, this face looks the same as face `region'.

 But when you use a command that's known to act on the active
 region, the mode-line indication instead uses face
 `mlr-region-acting-on', to draw attention to this fact.

 By a "command that acts on the active region" I mean a command
 that's "regionable": it acts on the region only when it's active.
 It typically acts on the full buffer, or the text from point to
 the end of the buffer, when the region's not active.

 And by "act" I mean the command is in some way region-aware,
 region-reactive, or region-sensitive.

 (A command that always acts on the region, whether or not it's
 active, can be called just "regional"; it's not "regionable".
 There's no need to avert you to its acting on the region.)

 Some specific commands and non-interactive functions are advised
 in `modeline-region-mode', to realize this feature of indicating
 that they act specially on the region.  These include replacement
 commands.

 If you use library `isearch+.el' then you can restrict Isearch
 commands and replacement commands to the active region (this is
 controlled by option `isearchp-restrict-to-region-flag').  Then,
 because such commands act on the region, the mode-line indication
 uses face `mlr-region-acting-on'.

 (Advice is also provided for the merely "regional" commands
 `prepend-to-buffer', `append-to-buffer', `copy-to-buffer',
 `append-to-file', and `write-region', but it isn't used.  To
 highlight the use of those commands as if they were "regionable",
 uncomment the code defining and using their advice.)

 You can show that other functions act on the region, besides those
 handled that way by default -
 see (@> "Show That Additional Commands Are Regionable").

(@* "Controlling What Region Info To Show")
 ** Controlling What Region Info To Show **

 These user options control what region size information is shown
 in the mode-line:

 * `mlr-region-style' - Show the size as the number of characters,
   bytes, lines & characters, rows &columns, or arbitrary info you
   choose.  For arbitrary info, you provide a format string and a
   Lisp sexp whose evaluation returns the value you want to show
   using that format.  (Arbitrary means arbitrary: the value
   displayed need not have anything to do with region size.)

   Use options `mlr-non-rectangle-style' and `mlr-rectangle-style'
   to specify just what to show for lines & characters and rows &
   columns, respectively.

 * `mlr-non-rectangle-style' - Show the number of lines & chars, or
   that plus the number of words in the region.

 * `mlr-rectangle-style' - Show the number of rectangle rows &
   columns, or that plus the number of words and characters.

 * `mlr-count-partial-words-flag' - Whether to count words that
   straddle rectangle rows.  By default they're not counted.

(@* "Use Mode-Line Menus To Change Behavior")
 ** Use Mode-Line Menus To Change Behavior **

 When `modeline-region-mode' or `global-modeline-region-mode' is
 enabled, the menu shown when you click the mode-line size fields
 lets you do the following, regardless of whether the region is
 currently active.  This is in addition to the usual operations of
 toggling display of column and line numbers, and toggling
 indication of buffer size:

  * `+ Region Info' - Command `mlr-toggle-non-rectangle-style':
    toggle the value of option `mlr-non-rectangle-style'.  This
    shows or hides the number of words in the region (in addition
    to the number of lines & characters).

  * `+ Rectangle Info' - Command `mlr-toggle-rectangle-style':
    toggle the value of option `mlr-rectangle-style'.  This shows
    or hides the number of words and characters in the rectangle
    (in addition to the number of rows & columns).

  * `Choose Region Style' - Command `mlr-choose-region-style':
    Choose what to show in the mode-line when the region is active.
    That is, change the value of option `mlr-region-style'.  (The
    value isn't saved.  Use `M-x customize-option' to save it.)

 When the region is active the menu also includes this item:

  * `Count Region Contents' - Command `count-words-region': Echo
    the number of lines, words, and characters in the region.

 When the active region is rectangular the menu also includes this
 item:

  * `Count Rectangle Contents' - Command
    `mlr-count-rectangle-contents': Echo the number of rows,
    columns, words, and characters in the rectangle.  By default,
    the count excludes words that straddle the rectangle row
    limits; with a prefix arg those partial words are counted.

 These items are also added to the mode-line menu for the size
 fields:

  * `Show Region Info Here' - Command `modeline-region-mode'.

  * `Show Region Info Globally' - Command
    `global-modeline-region-mode'.

 This means that you can use the mode-line size menus to toggle
 showing region info on and off - no need to invoke the mode
 commands from the keyboard to do that.

 All of these improvements to the mode-line size field menus are
 provided automatically.

 By default this is done just by loading this library.  But if you
 prefer not to have that done except on demand, then customize
 option `mlr-init-menu-on-load-flag' to `nil'.  If you do that then
 the menus will only be enhanced when you invoke
 `modeline-region-mode' or `global-modeline-region-mode' for the
 first time.

(@* "Highlight Large Column Numbers")
 ** Highlight Large Column Numbers **

 An unrelated additional mode-line enhancement is provided by
 option `mlr-column-limit'.  This highlights the current-column
 number in the mode-line whenever it's greater than the option
 value, letting you know when you've gone past a certain number of
 characters in any line.  Turn this option off if you don't want
 this behavior.

(@* "Show That Additional Commands Are Regionable")
 ** Show That Additional Commands Are Regionable **

 If you want additional commands to use face
 `mlr-region-acting-on', to draw attention to the fact that they're
 currently acting on the active region, there are two ways to make
 this happen.  You can use either approach for any command - in
 other words, you can mix and match if you like.

 1. Put non-nil property `mlr-acts-on' on the command symbol:

      (put 'my-cmd 'mlr-acts-on t)

 2. Advise the command, binding buffer-local variable
   `mlr-region-acting-on' to the value returned by `use-region-p'
   around the command's invocation.  Then add your advice function
   to hook `modeline-region-mode-hook'.

 #1 is the simplest approach, and is likely all you need.  To make
 use of it, turn on option `mlr-use-property-mlr-acts-on-flag'.

 [That option is off by default because you might never want to
 indicate additional regionable commands.  Also, `pre-command-hook'
 and `post-command-hook' are used when it's on, to check the
 current command for property `mlr-acts-on'.  Those hooks can
 sometimes slow things down or be a bother in other ways.  (That
 drawback isn't specific to this library.)]

 #2 is a bit more complicated, but it gives you additional control
 of the mode-line behavior.  You can also advise non-interactive
 functions (method #1 works only for commands).

 Here's an example of #2:

   (advice-add 'my-cmd :around #'my-advice)

   (defun my-advice (function &rest args)
     "Make mode-line show that FUNCTION acts on the active region."
     (cond (modeline-region-mode
            (let ((mlr-region-acting-on  (use-region-p)))
              (apply function args)))
           (t (apply function args))))

   (add-hook 'modeline-region-mode-hook 'my-advice)

 You can use the advice functions `mlr--advice-*' defined here as
 models, except that you'll want to make your advice functions
 conditional on whether `modeline-region-mode' is enabled, as in
 the above example.

 [The `modeline-region-mode' code itself adds and removes the advice
 functions defined here, depending on whether the mode is enabled.
 For example, like the `my-advice' example above, function
 `mlr--advice-3' binds `mlr-region-acting-on' to (use-region-p),
 but it does that unconditionally, since it's used only when the
 mode is enabled.]

 In your advice function, binding or setting `mlr-region-acting-on'
 to (use-region-p) is generally all that's needed, but in some
 cases you'll want an `:around' advice to also contain an
 `interactive' spec that's specific to the advised function.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
