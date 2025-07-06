
# 64tass-mode.el

Major mode for working with 64tass assembly, offering column-based editing. 

## Features

- Context-sensitive indentation and cycling through line segments
- Syntax highlighting/font-locking

## Commands and default keybindings

| Binding   | Command                      | Description                                          |
|-----------|------------------------------|------------------------------------------------------|
| `C-c C-b` | `64tass-insert-BASIC-header` | Insert BASIC loader stub                             |
| `<tab>`   | `64tass-align-and-cycle`     | Format the current line and move to the next segment |

## Custom variables

| Variable                           | Default | Description                                     |
|------------------------------------|---------|-------------------------------------------------|
| `64tass-instruction-column-indent` | `20`    | indent-level for assembly instructions          |
| `64tass-comment-column-indent`     | `40`    | indent-level for trailing/right-margin comments |

## License

© 2025 Sven Johansson. **GNU GPL v3.0** Licensed. See [`LICENSE`](LICENSE) for details.
