#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

$SCRIPT_DIR/emacs/install.sh
$SCRIPT_DIR/zsh/install.sh
$SCRIPT_DIR/tmux/install.sh
$SCRIPT_DIR/konsole/install.sh
