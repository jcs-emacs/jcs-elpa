Simple code formatter for Emacs Lisp.  Features:

- Won't format lines that end in '; nofmt'
- Focuses on the placement of lists and (mostly) ignores atoms
- Tries to break at `fill-column', but lines may exceed this number
  due to inline comments, long literals, trailing sequences of closed
  parens, or postprocessing (see `elfmt-autojoin-1' for example)
- Prefers "modern" Elisp (old-style backquotes will cause it to halt)

Usage:
- Use M-x elfmt to format the current buffer
- Use M-x elfmt-sexp to format the current sexp.
- Use M-x elfmt-mode to automatically format the buffer on save
- Use M-x elfmt-global-mode to enable `elfmt-mode' everywhere
