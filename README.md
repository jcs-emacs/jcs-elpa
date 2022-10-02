<picture>
  <source media="(prefers-color-scheme: light)" srcset="./docs/etc/logo/light/sink.png">
  <source media="(prefers-color-scheme: dark)" srcset="./docs/etc/logo/dark/sink.png">
  <img align="right" width="25%" src="">
</picture>

<a href="https://creativecommons.org/licenses/by-nc-nd/4.0/"><img src="https://img.shields.io/badge/License-CC_BY--NC--ND_4.0-lightgrey.svg" alt="License"></a>
<a href="https://www.gnu.org/software/emacs/download.html"><img src="https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/emacs.svg" alt="Emacs"></a>
<a href="#"><img src="https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/packages.svg" alt="Packages"></a>
<a href="#"><img src="https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/system.svg" alt="System"></a>

# jcs-elpa
> ELPA for [jcs-emacs](https://github.com/jcs-emacs/jcs-emacs), using [github-elpa](https://github.com/10sr/github-elpa)

[![Build](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/build.yml/badge.svg)](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/build.yml)
[![pages-build-deployment](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/pages/pages-build-deployment)
[![Install](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/install.yml/badge.svg)](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/install.yml)

[![System](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/system.yml/badge.svg)](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/system.yml)
[![Version](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/version.yml/badge.svg)](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/version.yml)
[![Packages](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/packages.yml/badge.svg)](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/packages.yml)

## 🏆 Goals

- Use built-in code over third-party library (`dash.el`, `s.el`, `f.el`, etc)
- Reuse third-party libraries if possible to keep the maintenance (think of the entire ELPA is a one huge piece of software), but not required
- Accept any packages, but they need to show some usefulness
- Don't remake the wheel, try to search for existing packages before you create one
- Contribute to existing packages that have similar features (unless the author refuses)
- Keep up the package's maintenance if possible, and ask someone for help if you need to

## 🔨 How to use?

Add the following to your configuration:

```elisp
(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
```

Use priority if you don't want this archive overridden with other larger archives:

```elisp
(setq package-archive-priorities '(("melpa"    . 5)
                                   ("jcs-elpa" . 0))
```

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either clone and make pull
requests to this repository. Or you can clone the project and establish your own
branch of this tool. Any methods are welcome!

## 📝 License

This work is licensed under the [CC BY-ND 4.0](https://creativecommons.org/licenses/by-nd/4.0/) license.

[![](https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png)](https://creativecommons.org/licenses/by-nd/4.0/)
[![License: CC BY-NC-ND 4.0](https://licensebuttons.net/l/by-nc-nd/4.0/80x15.png)](https://creativecommons.org/licenses/by-nc-nd/4.0/)
