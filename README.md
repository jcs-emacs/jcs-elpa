[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs Version](https://img.shields.io/badge/Emacs-27.1+-7F5AB6.svg?logo=gnu%20emacs&logoColor=white)](https://www.gnu.org/software/emacs/download.html)
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
