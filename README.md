
# 64tass-mode.el

[![Emacs: 30.0](https://img.shields.io/badge/Emacs-30.0-blue.svg)](https://www.gnu.org/software/emacs/)
![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)
![CI](https://github.com/svjson/64tass-mode.el/actions/workflows/test.yml/badge.svg)

Major mode for working with [64tass](https://github.com/irmen/64tass) assembly, offering column-based editing.


## Features

- Context-sensitive indentation and cycling through line segments.
- Syntax highlighting/font-locking.
- Invoking `64tass` for assembly from within Emacs.
- Assemble-and-launch with `64tass` and VICE (`x64`).
- Auto-detect entry-point file for assembly/launch.
- Built in memory map as contextual eldoc (based on https://sta.c64.org/cbm64mem.html )


## Related packages

This repository is a monorepo containing standalone packages for common functionality that
can be used by other modes or users that require these features, but can not or do not want to
depend on the full 64tass-mode.el major mode.

- [**`64tass-proc.el`**](packages/64tass-proc.el/README.md) - Utility package for interacting with/invoking `64tass`
- [**`flycheck-64tass.el`**](packages/flycheck-64tass.el/README.md) - Flycheck syntax checker for `64tass`.
- [**`vice-emu-proc.el`**](packages/vice-emu-proc.el/README.md) - Utility package for interacting with/invoking The VICE Emulator (`x64`).


## Commands and default keybindings

### Navigation and editing

| Binding     | Command                        | Description                                                     |   |
|-------------|--------------------------------|-----------------------------------------------------------------|---|
| `<tab>`     | `64tass-align-and-cycle`       | Format the current line and move to the next segment            |   |
| `<backtab>` | `64tass-align-and-cycle-left`  | Format the current line and move to the prevous segment         |   |
| `s-<up>`    | `64tass-shift-column-up`       | Shift the contents of the column at point upwards               |   |
| `s-<own>`   | `64tass-shift-column-down`     | Shift the contents of the column at point downwards             |   |
| `C-c C-n`   | `64tass-cycle-number-at-point` | Cycle through number formats/repesentations for number at point |   |


### Insert and generate

| Binding     | Command                        | Description                                                     |   |
|-------------|--------------------------------|-----------------------------------------------------------------|---|
| `C-c i h`   | `64tass-insert-BASIC-header`   | Insert BASIC loader stub                                        |
| `C-c i b`   | `64tass-insert-docblock`       | Insert docblock for the following line                          |


### Assembly / Launch

There is no canonical concept of a **project** in `64tass`, but `64tass-mode.el` provides a default implicit concept of a **64tass-project**.


| Binding     | Command                              | Description                                                                                                |
|-------------|--------------------------------------|------------------------------------------------------------------------------------------------------------|
| `C-c C-c`   | `64tass-assemble-project`            | Invoke `64tass` to assemble the **project** associated with the current buffer                             |
| `C-c C-e`   | `64tass-assemble-and-launch-project` | Assemble the **project** associated with the current buffer with `64tass` and launch program in VICE/`x64` |
| `C-c b C-c` | `64tass-assemble-buffer`             | Invoke `64tass` to assemble the **current buffer**                                                         |
| `C-c b C-e` | `64tass-assemble-and-launch-buffer`  | Assemble the **current buffer** with `64tass` and launch program in VICE/`x64`                             |


## Custom variables

| Variable                              | Type   | Description                                                                                         |
|---------------------------------------|--------|-----------------------------------------------------------------------------------------------------|
| `64tass-instruction-column-indent`    | `20`   | indent-level for assembly instructions                                                              |
| `64tass-comment-column-indent`        | `40`   | indent-level for trailing/right-margin comments                                                     |
| `64tass-default-project-resolver`     | \<fn>  | Default function to use for resolving the **project** associated with the current buffer            |
| `64tass-on-assembly-success-function` | \<fn>  | Callback to invoke upon successful assembly. Defaults to `64tass--on-assembly-success`.             |
| `64tass-on-assembly-error-function`   | \<fn>  | Callback to invoke upon assembly error. Defaults to `64tass--on-assembly-error`.                    |
| `64tass-label-format-function`        | \<fn>  | Determines label-style/format based on context. Defaults to `64tass-default-label-format-function`. |
| `64tass-xref-definition-source-order` | 'list  | Determines in which order sources/handlers will be invoked to find xref definitions                 |
| `64tass-xref-definition-source-alist` | 'alist | An alist containing (\<source symbol> . \<handler-function>) entries.                               |


## License

© 2025 Sven Johansson. **GNU GPL v3.0** Licensed. See [`LICENSE`](LICENSE) for details.
