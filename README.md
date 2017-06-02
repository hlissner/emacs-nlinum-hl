# nlinum-hl

Extends nlinum to provide current-line-number highlighting, as well as various
other fixes.

> This was once a part of [doom-themes]

## Install

`M-x package-install RET nlinum-hl`

```emacs-lisp
(require 'nlinum-hl)
(add-hook 'nlinum-mode #'nlinum-hl-mode)
```

## Configuration

The line number is highlighted with `nlinum-hl-face`, which can be customized.


[doom-themes]: https://github.com/hlissner/emacs-doom-theme
