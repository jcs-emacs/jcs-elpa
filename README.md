[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs Version](https://img.shields.io/badge/Emacs-27.2-7F5AB6.svg?logo=gnu%20emacs&logoColor=white)](https://www.gnu.org/software/emacs/download.html)
[![Linux](https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black)](#)

# elpa

[![Build](https://github.com/jcs090218/elpa/actions/workflows/build.yml/badge.svg)](https://github.com/jcs090218/elpa/actions/workflows/build.yml)

ELPA for [jcs-emacs](https://github.com/jcs090218/jcs-emacs), using [github-elpa](https://github.com/10sr/github-elpa)

## 🔨 How to use?

Add the following to your configuration:

```el
(setq package-archives
      `(,@package-archives
        ("jcs" . "https://jcs-emacs.github.io/elpa/elpa/")))
```

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)

If you would like to contribute to this project, you may either clone and make pull
requests to this repository. Or you can clone the project and establish your own
branch of this tool. Any methods are welcome!
