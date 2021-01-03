#!/bin/bash

sudo apt install emacs

if [ -h ~/.emacs.d ]; then
    # Remove if existing symlink
    rm ~/.emacs.d
elif [ -d ~/.emacs.d ]; then
    # Backup if existing directory
    mv ~/.emacs.d ~/.emacs.d.bak
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ln -s "$SCRIPT_DIR" ~/.emacs.d
