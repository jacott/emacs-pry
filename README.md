# Emacs Pry

* Git: http://github.com/jacott/emacs-pry
* Author: Geoff Jacobsen   
* Copyright: 2012
* License: GNU GPLv3

### Description

Emacs Pry is package that provides support for running [Pry](https://github.com/pry/pry) within in Emacs.

### Features

`run-pry` starts a Pry REPL inside a modified `term-mode` buffer.

`pry-intercept` command allows for quick debuging into a test.

You can use the pointer or other emacs commands to move the cursor on the command line and Pry will be aligned with the new position.

### Install

```bash
$ gem install pry
$ mkdir ~/.emacs.d/vendor
$  ~/.emacs.d/vendor
$ git clone git@github.com:jacott/emacs-pry
```

Add the following to your `.emacs` or `.emacs.d/init.el` file

```lisp
    (add-to-list 'load-path "~/.emacs.d/vendor/emacs-pry")
    (require 'pry)
    ;; optional suggestions
    (global-set-key [S-f9] 'pry-intercept)
    (global-set-key [f9] 'pry-intercept-rerun)
```

### Optional

* gem install pry-nav pry-stack_explorer

* https://github.com/jacott/Enhanced-Ruby-Mode

### Thanks

https://github.com/banister for the Pry gem!