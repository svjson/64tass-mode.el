
# flycheck-64tass.el

[![Emacs: 30.0](https://img.shields.io/badge/Emacs-30.0-blue.svg)](https://www.gnu.org/software/emacs/)
![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)

Flycheck syntax checker for 64tass [64tass](https://github.com/irmen/64tass) assembly files.

The checker uses the `64tass` binary on your system path. 
You must have it installed and accessible on your `PATH`.

## 64tass-mode.el

This syntax checker is automatically enabled in [`64tass-mode.el`](../../README.md), provided `flycheck` is installed and active.  
If you're using `64tass-mode`, no manual configuration is needed.

## Other modes

If you're using `asm-mode` or any other major mode and would like to enable this checker manually, you can do so by selecting it explicitly:

### Manually selecting the checker

You can tell Flycheck to use `64tass` in a specific buffer by running:

```elisp
M-x flycheck-select-checker RET 64tass RET
```

### Automatically enable it in `asm-mode`

If you want Flycheck to automatically use the 64tass checker in `asm-mode`, you can add this 
to your Emacs config:

```elisp
(add-hook 'asm-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-match "\\.\\(asm\\|s\\)$" buffer-file-name))
              (flycheck-select-checker '64tass)
              (flycheck-mode 1))))
```

You can customize the file name pattern as needed.


## Dependencies

- [Flycheck](https://www.flycheck.org/) must be installed and available in your Emacs configuration.
- `64tass` must be available in your system `PATH`, as the checker invokes it from the command line.

## Installation

This package depends on [Flycheck](https://www.flycheck.org), which is not included with Emacs by default.  
You can install it from MELPA:

```elisp
M-x package-install RET flycheck RET
```

Because `flycheck-64tass.el` lives in a monorepo structure under `./packages/flycheck-64tass`, it 
may not be directly compatible with `use-package`'s `:vc` keyword, which assumes the package file 
is at the repository root.

If you're not using a package manager like `straight.el`, clone or copy the file somewhere in 
your `load-path`, e.g, your `site-lisp` folder, and add the following to your init.el/.emacs

```elisp
(require 'flycheck-64tass)
```

To register the checker manually (if needed), call:

```elisp
(flycheck-64tass-setup)
```

### Using `straight.el`

If you're using [straight.el](https://github.com/radian-software/straight.el), you can install it like this:

```elisp
(straight-use-package
  '(flycheck-64tass :type git
                    :host github
                    :repo "svjson/64tass.el"
                    :files ("packages/flycheck-64tass/flycheck-64tass.el")))
```


## License

© 2025 Sven Johansson. **GNU GPL v3.0** Licensed. See [`LICENSE`](LICENSE) for details.
