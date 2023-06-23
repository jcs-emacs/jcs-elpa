use M-x `codeium-install' to install binaries automatically
add `codeium-completion-at-point' to your `completion-at-point-functions'
use `codeium-diagnose' to see currently enabled apis and fields

anything defined by `codeium-def' a constant or a function that
takes 1, 2, or 3 arguments, which are (api state val)
api is a symbol such as 'GetCompletions, state is of type `codeium-state'
which keeps all the state (including a process, port, and some hash tables)
val is only relevant if the field is only sensible given a previous
value, such as `codeium/request_id' used in `'CancelRequest'

use M-x `customize' see a full list of settings.
