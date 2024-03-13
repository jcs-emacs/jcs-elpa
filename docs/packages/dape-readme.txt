Dape is a debug adapter client for Emacs.  The debug adapter
protocol, much like its more well-known counterpart, the language
server protocol, aims to establish a common API for programming
tools.  However, instead of functionalities such as code
completions, it provides a standardized interface for debuggers.

To begin a debugging session, invoke the `dape' command.  In the
minibuffer prompt, enter a debug adapter configuration name from
`dape-configs'.

; For complete functionality, make sure to enable `eldoc-mode' in your
; source buffers and `repeat-mode' for more pleasant key mappings.

Package looks is heavily inspired by gdb-mi.el
