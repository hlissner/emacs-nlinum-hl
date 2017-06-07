[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/nlinum-hl-badge.svg)](http://melpa.org/#/nlinum-hl)

# nlinum-hl

<img src="https://raw.githubusercontent.com/hlissner/emacs-nlinum-hl/screenshots/nlinum-hl.png" align="right" />

> nlinum 1.7 is now available on ELPA and offers current-line highlighting, so
> this plugin has changed its focus.

`nlinum-hl` is an nlinum extension that tries to mitigate disappearing line
numbers in buffers that have been open a while (a known issue with nlinum).

> This was once a part of [doom-themes]

## Install

`nlinum-hl` is available on MELPA.

`M-x package-install RET nlinum-hl`

```emacs-lisp
(require 'nlinum-hl)

;; This seems to fix the problem completely, but I'm uncertain of the
;; performance ramifications. It might cause stuttering!
(add-hook 'post-gc-hook #'nlinum-hl-flush-all-windows)
```

The `post-gc-hook` hook works flawlessly for me. In case this isn't true for
everyone, here are some alternatives:

```emacs-lisp
;; whenever Emacs loses/gains focus
(add-hook 'focus-in-hook  #'nlinum-hl-flush-all-windows)
(add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows)

;; when idling
(run-with-idle-timer 5 t #'nlinum-hl-flush-window)
(run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)

;; when switching windows
(advice-add #'select-window :before #'nlinum-hl-do-flush)
(advice-add #'select-window :after  #'nlinum-hl-do-flush)
```


[doom-themes]: https://github.com/hlissner/emacs-doom-themes
