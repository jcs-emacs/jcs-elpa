This file defines a generic interface for LLMs (large language models), and
functionality they can provide.  Not all LLMs will support all of these, but
programs that want to integrate with LLMs can code against the interface, and
users can then choose the LLMs they want to use.  It's advisable to have the
possibility of using multiple LLMs when that make sense for different
functionality.

Users should require this module and then the module of the LLM they want to
use.

Not all LLMs might be able to do everything, so clients need to catch any
signals thrown with symbol `not-implemented', and surface an error to the
user that the LLM they have chosen cannot be used for that functionality.
