[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/solaire-mode-badge.svg)](http://melpa.org/#/solaire-mode)

# nlinum-hl

Extends nlinum to provide current-line-number highlighting, plus other fixes.

It also tries to stave off a nlinum glitch where line numbers disappear (usually
in buffers that have been open a while).

> This was once a part of [doom-themes]

## Install

`M-x package-install RET nlinum-hl`

```emacs-lisp
(require 'nlinum-hl)
(add-hook 'nlinum-hook #'nlinum-hl-mode))
```

Alternatively, use `use-package`:

```emacs-lisp
(use-package nlinum-hl
  :after nlinum
  :config
  (add-hook 'nlinum-hook #'nlinum-hl-mode))
```

## Configuration

+ Customize `nlinum-hl-face` to change how it looks.


[doom-themes]: https://github.com/hlissner/emacs-doom-themes
