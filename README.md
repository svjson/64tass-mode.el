
# 64tass-mode.el

[![Emacs: 30.0](https://img.shields.io/badge/Emacs-30.0-blue.svg)](https://www.gnu.org/software/emacs/)
![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)

Major mode for working with [64tass](https://github.com/irmen/64tass) assembly, offering column-based editing.

## Features

- Context-sensitive indentation and cycling through line segments.
- Syntax highlighting/font-locking.
- Invoking `64tass` for assembly from within Emacs.

## Related packages

This repository is a monorepo containing standalone packages for common functionality that
can be used by other modes or users that require these features, but can not or do not want to
depend on the full 64tass-mode.el major mode.

- [**`64tass-proc.el`**](packages/64tass-proc.el/README.md) - Utility package for interacting with/invoking `64tass`
- [**`flycheck-64tass.el`**](packages/flycheck-64tass.el/README.md) - Flycheck syntax checker for `64tass`.

## Commands and default keybindings

| Binding     | Command                       | Description                                             |
|-------------|-------------------------------|---------------------------------------------------------|
| `C-c C-b`   | `64tass-insert-BASIC-header`  | Insert BASIC loader stub                                |
| `C-c C-c`   | `64tass-assemble-buffer`      | Invoke `64tass` to assemble the current buffer          |
| `<tab>`     | `64tass-align-and-cycle`      | Format the current line and move to the next segment    |
| `<backtab>` | `64tass-align-and-cycle-left` | Format the current line and move to the prevous segment |


## Custom variables

| Variable                              | Default | Description                                                                             |
|---------------------------------------|---------|-----------------------------------------------------------------------------------------|
| `64tass-instruction-column-indent`    | `20`    | indent-level for assembly instructions                                                  |
| `64tass-comment-column-indent`        | `40`    | indent-level for trailing/right-margin comments                                         |
| `64tass-on-assembly-success-function` | <fn>    | Callback to invoke upon successful assembly. Defaults to `64tass--on-assembly-success`. |
| `64tass-on-assembly-error-function`   | <fn>    | Callback to invoke upon assembly error. Defaults to `64tass--on-assembly-error`.        |

## License

© 2025 Sven Johansson. **GNU GPL v3.0** Licensed. See [`LICENSE`](LICENSE) for details.
