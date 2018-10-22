[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/copyleft/gpl.html)

# emacs-find-file-rg

This package allows to find file in current project using `rg --files` command.

Ido completion read will be used if `ido-mode` is active.

## Installation

### With `package.el`

Download `find-file-rg.el` and run:

<kbd>M-x</kbd> `package-install-file` <kbd>RET</kbd> `<path-to-find-file-rg.el>` <kbd>RET</kbd>

### Without `package.el`

Put `find-file-rg.el` somewhere in your load path and add this to `init.el`:

``` el
(require 'find-file-rg)
```

## Usage

### Interactive commands

#### `find-file-rg`

Asks for project dir if needed, reads files list using `rg --files --follow` command and reads file with completing function. Always asks for directory to find file in if invoked with prefix argument.

Accepts optional arg `initial`. It's value used as initial input for completing function.

#### `find-file-rg-at-point`

Calls `find-file-rg` with active region or filename at point as initial value for completing function.
