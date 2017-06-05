[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/solaire-mode-badge.svg)](http://melpa.org/#/solaire-mode)

# nlinum-hl

![nlinum-hl screenshot](/../screenshots/nlinum-hl.png)

Extends nlinum to provide current-line-number highlighting, and tries to
mitigate disappearing line numbers (a known issue with nlinum). Read more on
this below.

nlinum 1.7 offers current-line highlighting, but as of this writing only 1.6 is
available on ([M]ELPA|Marmalade). I also believe my implementation to be faster.

> This was once a part of [doom-themes]

## Install

`M-x package-install RET nlinum-hl`

```emacs-lisp
(require 'nlinum-hl)
(add-hook 'nlinum-mode-hook #'nlinum-hl-mode)
```

Alternatively, use `use-package`:

```emacs-lisp
(use-package nlinum-hl
  :after nlinum
  :config
  (add-hook 'nlinum-mode-hook #'nlinum-hl-mode))
```

## Configuration

+ Customize `nlinum-hl-face` to change how it looks.


## Disappearing line numbers

Occasionally, `nlinum`'s line numbers will disappear in buffers that have been
open a while.

When the current line is missing a line number, `nlinum-hl` can deal with it
depending on the value of `nlinum-hl-redraw`. Its possible values are:

+ `'line`: fix only that line's number (fastest)
+ `'window` (the default): redraw the visible part of the current window
+ `'buffer`: redraw all line numbers in that buffer
+ `t`: redraw nlinum across all buffers with nlinum-mode active (slowest)

In the interest of performance, this is all nlinum-hl will do for you. This may
not be enough for some, so here are more things you can try:

```emacs-lisp
;; whenever Emacs loses/gains focus
(add-hook 'focus-in-hook  #'nlinum-hl-flush-all-windows)
(add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows)

;; when idling
(run-with-idle-timer 5 t #'nlinum-hl-flush-window)
(run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)

;; when switching windows
(advice-add #'select-window :before #'nlinum-hl-flush)
(advice-add #'select-window :after  #'nlinum-hl-flush)

;; this seems to fix the problem completely, but I'm uncertain of the
;; performance ramifications. It might cause stuttering!
(add-hook 'post-gc-hook #'nlinum-hl-flush-all-windows)
```


[doom-themes]: https://github.com/hlissner/emacs-doom-themes
