
# 64tass-proc.el

[![Emacs: 30.0](https://img.shields.io/badge/Emacs-30.0-blue.svg)](https://www.gnu.org/software/emacs/)
![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)

This package contains utility functions for invoking the [64tass](https://github.com/irmen/64tass) 
binary from within emacs. It is designed as a standalone package, in the sense that it can be used
by any package that whishes to do so, without depending on `64tass-mode.el`.

## Functions of interest

### `64tass-exec`

General-purpose function for invoking the 64tass binary.
Returns a cons cell of the exit code and the console output.

```elisp
(64tass-exec '("myprog.asm" "-o" "myprog.prg") t t)
```

*Arguments:*  
0 `args` - list of string - command arguments  
1 `include-proc-args` - boolean - prepend default arguments when non-nil  
2 `append-to-log` - boolean/string - non-nil appends to log buffer, string overrides buffer name  


### `64tass-assemble-buffer`

Invokes the `64tass` binary to assemble the current buffer and write the output to a file.
By default, the output file will have the same name as the source file but with the file
extension replaced with `.prg`.

The invocation behavior can be controller with the local variables `64tass-proc-output-file`
and `64tass-proc-args`. The custom variable `64tass-proc-output-file-extension` can be 
customized or shadowed to control the file-extension of the assembled output.

Packages, buffers or users that need to react to success or failure of the assembly can 
customize or shadow the callback functions `64tass-proc-on-assembly-success-function` 
and/or `64tass-proc-on-assembly-error-function` as needed.

**Example**
```elisp
(setq-local 64tass-proc-on-assembly-success-function #'my-64tass-success-function)
```

Using `setq-local` will shadow any global default or customized value.

The function internally uses `64tass-proc--parse-assembly-output` to parse the raw
console output of the 64tass assembly operation into a structured plist format.

**Example of assembly success**

```elisp
(:input-file "/tmp/labs/test1/test.asm"
 :output-file "/tmp/labs/test1/test.prg"
 :segments ((:type :data
             :length 13
             :start (:hex "$0801" :dec 2049)
             :end (:hex "$080d" :dec 2061))
            (:type :gap
             :length 2
             :start (:hex "$080e" :dec 2062)
             :end (:hex "$080f" :dec 2063))
            (:type :data
             :length 11
             :start (:hex "$0810" :dec 2064)
             :end (:hex "$081a" :dec 2074))
            (:type :passes
             :n 2)))
```

** Example of assembly with errors **

```elisp
(:input-file "/tmp/labs/test1/test.asm"
 :error-count 1
 :errors ((:input-file "/tmp/labs/test1/test.asm"
           :line 13
           :column 23
           :message "wrong type 'bits'"
           :source-ref ("                 cpxl #$ff"
                        "                       ^")))
 :segments ((:type :passes
             :n 2)
            (:type :data
             :length 11
             :start (:hex "$0810" :dec 2064)
             :end (:hex "$081a" :dec 2074))
            (:type :passes
             :n 2)))
```

### `64tass-dump-labels`

Dumps label definitions for the source file in the current buffer and parses it into an
alist in a format suitable for label/identifier lookup:

```elisp
(("clear_screen" . (:file "main.asm" :linum "396" :addr "$0a57"))
 ("ball_cell_x" . (:file "main.asm" :linum "576" :addr "$0b89"))
 ("setup_game_mode" . (:file "main.asm" :linum "22" :addr "$081d"))
 ("read_joystick_port2" . (:file "main.asm" :linum "365" :addr "$0a3f"))
 ("brick_anim_tick" . (:file "main.asm" :linum "711" :addr "$400f"))
 ("game_irq_tick" . (:file "main.asm" :linum "93" :addr "$088e"))
 ("level_1_data" . (:file "main.asm" :linum "681" :addr "$0be4"))
 ...)
```

## Buffer-local variables

### `64tass-proc-output-file`
**Type**: `string`  
**Default**: `nil`  

Controls the full name given to the assembled binary. This should contain the full file name
including the file extension. If this variable has a `non-nil` value, no extension substitution
will be performed and the custom variable `64tass-proc-output-file-extension` will be ignored.

### `64tass-proc-args`
**Type**: `list`  
**Default**: `nil`  

Additional arguments and flags used to invoke the 64tass process for assembly, in addition
to the input/output file arguments that are automatically assembled.

## Custom Variables

### `64tass-proc-output-file-extension`
**Type**: `string`  
**Default**: `".prg"`  

Controls the name given to the assembled binary, unless the output file name is set in full
by `64tass-proc-output-file`.

### `64tass-proc-on-assembly-success-function`
**Type**: `function`  
**Default**: `#'ignore`  

Callback function to be invoked when assembly is successful. The result of the assembly
operation is passed as the single argument to the configured function. 

See `64tass-assemble-function` for the argument format.

### `64tass-proc-on-assembly-error-function`

**Type**: `function`  
**Default**: `#'ignore`  

Callback function to be invoked when assembly results in an error. The result of the assembly
operation is passed as the first argument to the configured function, and the first error
encountered is passed as the second argument. The full error information is available in
the first argument.

See `64tass-assemble-function` for the argument format.

### `64tass-proc-log-buffer-name`

**Type**: `string`  
**Default**: `"*64tass assembly log*"`  

Name of the log buffer where console output ends up.


## License

© 2025 Sven Johansson. **GNU GPL v3.0** Licensed. See [`LICENSE`](LICENSE) for details.
