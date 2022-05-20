<p align="center">
<img src="./docs/etc/sink_black.png#gh-light-mode-only" width="30%"/>
<img src="./docs/etc/sink_white.png#gh-dark-mode-only" width="30%"/>
</p>

<p align="center">
<a href="https://creativecommons.org/licenses/by-nc-nd/4.0/"><img src="https://img.shields.io/badge/License-CC_BY--NC--ND_4.0-lightgrey.svg" alt="License"></a>
<a href="https://www.gnu.org/software/emacs/download.html"><img src="https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/emacs.svg" alt="Emacs"></a>
<a href="#"><img src="https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/packages.svg" alt="Packages"></a>
</p>

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [jcs-elpa](#jcs-elpa)
    - [🔨 How to use?](#🔨-how-to-use)
    - [Contribute](#contribute)
    - [📝 License](#📝-license)

<!-- markdown-toc end -->

# jcs-elpa

[![Build](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/build.yml/badge.svg)](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/build.yml)
[![pages-build-deployment](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/pages/pages-build-deployment)
[![System](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/system.yml/badge.svg)](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/system.yml)
[![Version](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/version.yml/badge.svg)](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/version.yml)
[![Packages](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/packages.yml/badge.svg)](https://github.com/jcs-emacs/jcs-elpa/actions/workflows/packages.yml)

ELPA for [jcs-emacs](https://github.com/jcs-emacs/jcs-emacs), using [github-elpa](https://github.com/10sr/github-elpa)

## 🔨 How to use?

Add the following to your configuration:

```elisp
(setq package-archives
      `(,@package-archives
        ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))
```

Use priority if you don't want this archive overridden with other larger archives:

```elisp
(setq package-archive-priorities '(("melpa"    . 5)
                                   ("jcs-elpa" . 0))
```

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)

If you would like to contribute to this project, you may either clone and make pull
requests to this repository. Or you can clone the project and establish your own
branch of this tool. Any methods are welcome!

## 📝 License

This work is licensed under the [CC BY-ND 4.0](https://creativecommons.org/licenses/by-nd/4.0/) license.

[![](https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png)](https://creativecommons.org/licenses/by-nd/4.0/)
[![License: CC BY-NC-ND 4.0](https://licensebuttons.net/l/by-nc-nd/4.0/80x15.png)](https://creativecommons.org/licenses/by-nc-nd/4.0/)
