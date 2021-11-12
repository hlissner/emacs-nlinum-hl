[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/nlinum-hl-badge.svg)](http://melpa.org/#/nlinum-hl)
![nlinum-hl](https://img.shields.io/badge/nlinum--hl-v1.0.5-blue.svg)

> **This package is no longer maintained** now that Emacs 26+ has  native line
> numbers (see `display-line-numbers-mode`), which is superior to nlinum in
> every way (and does not suffer the issue this package was made to address).
> Use that instead!

# nlinum-hl

<img src="/../screenshots/nlinum-hl.png" align="right" />

> nlinum 1.7 is now available on ELPA and offers current-line highlighting, so
> this plugin has changed its focus.

`nlinum-hl` [tries to] remedy an issue in `nlinum` where line numbers disappear,
due to a combination of bugs internal to nlinum and the fontification processes
of certain major-modes and commands.

> This was once a part of [doom-themes]

## Install

`nlinum-hl` is available on MELPA.

`M-x package-install RET nlinum-hl`

```emacs-lisp
(require 'nlinum-hl) ; load this after nlinum
```

90% of the time this should be all you need.

However, certain major-modes, commands and functions will still eat up line
numbers, typically as a result of using `with-silent-modifications` or
preventing jit-lock from detecting changes in general.

In that case, this package provides these functions:

+ `nlinum-hl-flush-region`: flush a specific region in the current window.
+ `nlinum-hl-flush-all-windows`: flush all open windows.
+ `nlinum-hl-flush-window`: flush the current window.

Here are some examples of how to use them:

```emacs-lisp
;; A shotgun approach that refreshes line numbers on a regular basis:
;; Runs occasionally, though unpredictably
(add-hook 'post-gc-hook #'nlinum-hl-flush-all-windows)

;; whenever Emacs loses/gains focus
(add-hook 'focus-in-hook  #'nlinum-hl-flush-all-windows)
(add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows)
;; ...or switches windows
(advice-add #'select-window :before #'nlinum-hl-do-select-window-flush)
(advice-add #'select-window :after  #'nlinum-hl-do-select-window-flush)

;; after X amount of idle time
(run-with-idle-timer 5 t #'nlinum-hl-flush-window)
(run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)

;; Bind it for flush-on-demand (this might be excessive)
(global-set-key (kbd "<f9>") #'nlinum-hl-flush-all-windows)
(global-set-key (kbd "<f8>") #'nlinum-hl-flush-window)
(global-set-key (kbd "<f7>") #'nlinum-hl-flush-region) ; on selections
```

Also included are a variety of `nlinum-hl-do-*` advice functions, meant to be
attached to certain functions with `advice-add`.

Here are all the known issues and fixes (feel free to report/contribute more):

```emacs-lisp
;; With `markdown-fontify-code-blocks-natively' enabled in `markdown-mode',
;; line numbers tend to vanish next to code blocks.
(advice-add #'markdown-fontify-code-block-natively
            :after #'nlinum-hl-do-markdown-fontify-region)

;; When using `web-mode's code-folding an entire range of line numbers will
;; vanish in the affected area.
(advice-add #'web-mode-fold-or-unfold :after #'nlinum-hl-do-generic-flush)

;; Changing fonts can leave nlinum line numbers in their original size; this
;; forces them to resize.
(add-hook 'after-setting-font-hook #'nlinum-hl-flush-all-windows)
```


[doom-themes]: https://github.com/hlissner/emacs-doom-themes
