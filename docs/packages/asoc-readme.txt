Emacs Lisp provides only minimal facilities for dealing with alists, and
processing them using standard list functions is awkward and tedious. `asoc`
provides a complete API for handling association lists. In addition to basic
functions for creating, accessing and modifying alists, it provides mapping,
filtering and folding facilities in both regular and anaphoric variants, a
looping construct analogous to `dolist`, and a special variable for
configuring the equality predicate used by `asoc` operations.

