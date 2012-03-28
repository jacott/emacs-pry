# Emacs Pry

* Git: http://github.com/jacott/emacs-pry
* Author: Geoff Jacobsen   
* Copyright: 2012
* License: GNU GPLv3

### Description

Emacs Pry is package that provides support for running Pry within in Emacs.

### Features

`run-pry` starts a pry REPL inside a modified `term-mode` buffer.

`pry-intercept` command allows for quick debuging into a test.

### Install

* git clone git@github.com:jacott/emacs-pry

* gem install pry

* Add the following to your `.emacs` or `init.el` file

```lisp
    (add-to-list 'load-path "(path-to)/emacs-pry")
    (require 'pry)
    ;; optional suggestions
    (global-set-key [f9] 'pry-intercept)
    (global-set-key [S-f9] 'pry-intercept-nonstop)
```

### Optional

* gem install pry-nav pry-stack_explorer

* https://github.com/jacott/Enhanced-Ruby-Mode

### Development

Testing parser:

ruby -I. test/test_erm_buffer.rb

Sorry, no test suite for ruby-mode.el

### Thanks

https://github.com/banister for the pry gem!