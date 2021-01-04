#!/bin/bash

cp konsole/EmacsBase.keytab konsole/Emacs.keytab
emacs --script ~/.emacs.d/append-to-keytab.el
