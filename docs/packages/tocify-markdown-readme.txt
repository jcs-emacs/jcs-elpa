Generate a TOC from a markdown file: M-x tocify-markdown-generate-toc
This will compute the TOC at insert it at current position.
Update existing TOC: C-u M-x tocify-markdown-generate-toc

Here is a possible output:
<!-- tocify-markdown start - Don't edit this section. Run M-x tocify-markdown-refresh-toc -->
**Table of Contents**

- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
  - [Sources](#sources)
    - [Marmalade (recommended)](#marmalade-recommended)
    - [Melpa-stable](#melpa-stable)
    - [Melpa (~snapshot)](#melpa-~snapshot)
  - [Install](#install)
    - [Load org-trello](#load-org-trello)
  - [Alternative](#alternative)
    - [Git](#git)
    - [Tar](#tar)
- [another title](#another-title)
  - [with](#with)
  - [some](#some)
- [heading](#heading)

<!-- tocify-markdown end -->

Install - M-x package-install RET tocify-markdown RET
