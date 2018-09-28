# emacs-find-file-rg

This package allows to find file in current project using rg --files command.

Ido completion read will be used if `ido-mode` is active.

## Installation

### With `package.el`

Download `find-file-rg.el` and run:
```el
M-x package-install-file RET <path-to-find-file-rg.el> RET
```

### Without `package.el`

Put `find-file-rg.el` somewhere in your load path and add this to `init.el`:

``` el
(require 'find-file-rg)
```

## Usage

### Interactive commands

#### `find-file-rg`

Asks for project dir if needed, read files list using `rg --files --follow` command and reads file with completing function.

Accepts optional arg `initial`. It's value used as initial input for completing function.

### `find-file-rg-at-point`

Calls `find-file-rg` with active region or filename at point as initial value for completing function.
