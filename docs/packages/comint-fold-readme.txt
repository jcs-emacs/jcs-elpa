comint-fold configures hideshow mode to fold comint input + output
blocks, optionally binding the Tab key to fold such blocks prior
to the prompt, and with a fringe indicator for folded blocks.

Configuration:

Normally, `comint-prompt-regexp' is used to identify the input
prompts, but this can be altered; see `comint-fold-prompt-regexp'.
In addition, blank lines prior to the prompt can be preserved
outside of the hidden block; see `comint-fold-blank-lines'.  The
easiest way to configure these variables locally for some comint
mode is to add the lambda returned by `comint-fold-configure-hook'
to the relevant mode hook.
