
Sometimes in a long undo chain where Emacs jumps to a position, I
can’t tell whether the undo operation just moved to this position
or it has also deleted some text. This package is meant to
alleviate that confusion: it flashes the to-be-deleted text before
deleting so I know what is happening.

This package is pretty efficient, I can hold down undo button and
the highlight doesn’t slow down the operation.
