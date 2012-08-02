# Emacs Pry

* Git: http://github.com/jacott/emacs-pry
* Author: Geoff Jacobsen
* Copyright: 2012
* License: GNU GPLv3

### Description

Emacs Pry is package that provides support for running [Pry](https://github.com/pry/pry) within in Emacs.

### Features

`run-pry` starts a Pry REPL inside a modified `term-mode` buffer.

`pry-intercept` command allows for quick debuging into a test without needing to write the `binding.pry` into the file.

Automatically shows source in emacs buffer.

You can use the pointer or other emacs commands to move the cursor on the command line and Pry will be aligned with the new position.

### Install

Ensure you are running ruby 1.9.3 or later

```sh
gem install pry
mkdir -p ~/.emacs.d/vendor
cd ~/.emacs.d/vendor
git clone git@github.com:jacott/emacs-pry
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

* `gem install pry-nav pry-stack_explorer`
* `gem install termios # replaces the running of: stty sane`

* https://github.com/jacott/Enhanced-Ruby-Mode

### Thanks

https://github.com/banister for the Pry gem!