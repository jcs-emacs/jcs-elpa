indent-bars highlights indentation with configurable font-lock
based vertical bars, using stipples.  The color and appearance
(weight, pattern, position within the character, zigzag, etc.) are
all configurable.  Includes the option for depth-varying colors and
highlighting the indentation level of the current line.  Bars span
blank lines, by default.  indent-bars works in any mode using fixed tab
or space-based indentation.  In the terminal (or on request) it
uses vertical bar characters instead of stipple patterns.

For Developers:

To efficiently accommodate simultaneous alternative bar styling, we
do two things:

 1. Collect all the style related information (color, stipple
    pattern, etc.) into a single struct, operating on one such
    "current" style struct at a time.

 2. Provide convenience functions for replicating "alternative"
    custom variables the user can configure; see
    `indent-bars--style'.  These variables can "inherit" nil or
    omitted plist variables from their parent var.

To temporarily alter the current style, it's enough to bind the
variable `indent-bars-current-style' dynamically.

Note the shorthand substitution for style related slot;
see file-local-variables at the end:

   ibs/  => indent-bars-style-
