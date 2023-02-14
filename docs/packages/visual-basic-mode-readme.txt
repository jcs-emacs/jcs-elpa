Purpose of this package:
 This is a mode for editing programs written in The World's Most
 Successful Programming Language.  It features automatic
 indentation, font locking, keyword capitalization, and some minor
 convenience functions.

Installation instructions
 Put visual-basic-mode.el somewhere in your path, compile it, and add
 the following to your init file:

 (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
 (push '("\\.\\(?:frm\\|\\(?:ba\\|cl\\|vb\\)s\\)\\'" . visual-basic-mode)
        auto-mode-alist)

 If you are doing Rhino scripts, add instead:
 (push '("\\.\\(?:frm\\|\\(?:ba\\|cl\\|vb\\)s\\|rvb\\)\\'" . visual-basic-mode)
        auto-mode-alist)

 If you had visual-basic-mode already installed, you may need to call
 visual-basic-upgrade-keyword-abbrev-table the first time that
 visual-basic-mode is loaded.

Of course, under Windows 3.1, you'll have to name this file
something shorter than visual-basic-mode.el

Revisions:
1.0 18-Apr-96  Initial version
1.1 Accomodate Emacs 19.29+ font-lock-defaults
    Simon Marshall <Simon.Marshall@esrin.esa.it>
1.2 Rename to visual-basic-mode
1.3 Fix some indentation bugs.
1.3+ Changes by Dave Love: [No attempt at compatibility with
     anything other than Emacs 20, sorry, but little attempt to
     sanitize for Emacs 20 specifically.]
     Change `_' syntax only for font-lock and imenu, not generally;
     provide levels of font-locking in the current fashion;
     font-lock case-insensitively; use regexp-opt with the font-lok
     keywords; imenu support; `visual-basic-split-line', bound to
     C-M-j; account for single-statement `if' in indentation; add
     keyword "Global"; use local-write-file-hooks, not
     write-file-hooks.
1.4 September 1998
1.4 KJW Add begin..end, add extra keywords
    Add customisation for single line if.  Disallow by default.
    Fix if regexp to require whitespace after if and require then.
    Add more VB keywords.  Make begin..end work as if..endif so
    that forms are formatted correctly.
1.4.1 KJW Merged Dave Love and KJW versions.
    Added keywords suggested by Mickey Ferguson
    <MFerguson@peinc.com>
    Fixed imenu variable to find private variables and enums

    Changed syntax class of =, <, > to punctuation to allow dynamic
    abbreviations to pick up only the word at point rather than the
    whole expression.

    Fixed bug introduced by KJW adding suport for begin...end in
    forms whereby a single end outdented.

    Partially fixed failure to recognise if statements with
    continuations (still fails on 'single line' if with
    continuation, ugh).
1.4.2 RF added "class" and "null" keywords, "Rhino" script note.
1.4.3 VB1 added
    1) function visual-basic-if-not-on-single-line to recognize single line
     if statements, even when line is broken.  variable
     visual-basic-allow-single-line-if default set to t again.
    2) use of 'words in calling regexp-opt rather than concat \\< ...\\>
    3) new keywords Preserve and Explicit
1.4.4 VB1 added function visual-basic-close-block
1.4.5 VB1, (expand-abbrev) within (save-excusion...)
1.4.6 VB1 correct visual-basic-close-block (single line If case)
1.4.7 VB1 correct visual-basic-close-block (For/Next)
1.4.8 VB1 correct visual-basic-close-block (Property, + add With /End With)
          add command visual-basic-insert-item
1.4.8 2010-05-15 Lennart Borgman:
       - Minor corrections
1.4.9 VB1 - make customizable variable accessible through defcustom
          - add support for `Select Case' in visual-basic-insert-item
          - reword of the `Dim' case in  visual-basic-insert-item
1.4.9b Lennart Borgman+VB1: correct abbreviation and support `_' as a valid
       symbol character
1.4.10 VB1 - Add punctuation syntax for operators
           - create visual-basic-check-style
           - improve idiom detection
1.4.10b,c VB1 -improve visual-basic-check-style
1.4.10d   VB1 -correct font lock keywords for case
              -improve visual-basic-check-style + add highlight overlay
1.4.11 Wang Yao - correct the regular expression for imenu
                - remove the string-to-char for imenu-syntax-alist, for xemacs error
                - change the condition of visual-basic-enable-font-lock which prevents emacs from running in command-line mode when the emacs-version is 19.29
                - correct the implement of droping tailing comment in visual-basic-if-not-on-single-line
1.4.12 VB1 - add visual-basic-propertize-attribute
1.4.13 VB1 - set default indentation to 3 char to stick to http://en.wikibooks.org/wiki/Visual_Basic/Coding_Standards#White_Space_and_Indentation
1.5    VB1 - Make the indentation of defun's recursive, i.e. a Sub defined within a Class will be indented by one indentatiation.


Notes:
Dave Love
BTW, here's a script for making tags tables that I (Dave Love) have
used with reasonable success.  It assumes a hacked version of etags
with support for case-folded regexps.  I think this is now in the
development version at <URL:ftp://fly.cnuce.cnr.it/pub/> and should
make it into Emacs after 20.4.

#! /bin/sh

# etags-vb: (so-called) Visual (so-called) Basic TAGS generation.
# Dave Love <d.love@dl.ac.uk>.  Public domain.
# 1997-11-21

if [ $# -lt 1 ]; then
    echo "Usage: `basename $0` [etags options] VBfile ... [etags options] " 1>&2
    exit 1
fi

if [ $1 = "--help" ] || [ $1 = "-h" ]; then
    echo "Usage: `basename $0` [etags options] VBfile ... [etags options]

"
    etags --help
fi

exec etags --lang=none -c '/\(global\|public\)[ \t]+\(\(const\|type\)[ \t]+\)*\([a-z_0-9]+\)/\4/' \
    -c '/public[ \t]+\(sub\|function\|class\)[ \t]+\([a-z_0-9]+\)/\2/' \
  "$@"

End Notes Dave Love


Known bugs:
 Doesn't know about ":" separated stmts



todo:
 fwd/back-compound-statement
 completion over OCX methods and properties.
 IDE integration
 Change behaviour of ESC-q to recognise words used as paragraph
 titles and prevent them being dragged into the previous
 paragraph.
 etc.
