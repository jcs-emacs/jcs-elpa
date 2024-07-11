`eglot-copilot-setup' Handles integration with emacs.
<example>
(eglot-copilot-setup)
</example>
`eglot-copilot-node-agent-script' will need to be updated to point to an copilot agent.js.
An example can be found at https://github.com/github/copilot.vim/blob/release/copilot/dist/agent.js

This package provides a company transformer named `eglot-copilot-sort-results-company-transformer'
which takes the results of the copilot panel, and sorts the company candidates based on their presence in
the copilot panel.
It is added to the transformer list by default in `eglot-copilot-setup'
