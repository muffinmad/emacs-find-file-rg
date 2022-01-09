[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/copyleft/gpl.html)
[![MELPA](https://melpa.org/packages/find-file-rg-badge.svg)](https://melpa.org/#/find-file-rg)

# emacs-find-file-rg

This package allows to find file in current project or any directory using `rg --files` command.

## Why?

As a [ripgrep](https://github.com/BurntSushi/ripgrep) user, I have fine-tuned `.ignore` file in my projects folders to exclude certain files from grepping. It turned out that `rg --files` provides the list of only interesting files.

## Usage

### Interactive commands

#### `find-file-rg`

Asks for project dir if needed and reads filename with completing function. `project-current` is used as the default directory to search in. If invoked with prefix argument, always asks for directory to find files in.

#### `find-file-rg-at-point`

Calls `find-file-rg` with active region or filename at point as initial value for completing function.

### Keybindings

No predefined keybindings are provided. I personally use <kbd>C-c f</kbd> for `find-file-rg` and <kbd>C-c g</kbd> for `find-file-rg-at-point`.

## Installation

### With `package.el`

Download `find-file-rg.el` and run:

<kbd>M-x</kbd> `package-install-file` <kbd>RET</kbd> `<path-to-find-file-rg.el>` <kbd>RET</kbd>

### Without `package.el`

Put `find-file-rg.el` somewhere in your load path and add this to `init.el`:

``` el
(require 'find-file-rg)
```
