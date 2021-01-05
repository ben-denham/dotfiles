#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $SCRIPT_DIR

cp konsole/EmacsBase.keytab konsole/Emacs.keytab
emacs --script ~/.emacs.d/append-to-keytab.el
