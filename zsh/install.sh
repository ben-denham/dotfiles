#!/bin/bash

sudo apt install zsh

if [ -h ~/.zshrc ]; then
    # Remove if existing symlink
    rm ~/.zshrc
elif [ -f ~/.zshrc ]; then
    # Backup if existing file
    mv ~/.zshrc ~/.zshrc.bak
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ln -s "$SCRIPT_DIR"/.zshrc ~/.zshrc

# Create terminfo for 24-bit colour terminal
/usr/bin/tic -x -o ~/.terminfo $SCRIPT_DIR/xterm-24bit.terminfo

chsh -s /usr/bin/zsh
echo "For your shell to be changed to zsh, you'll need to logout and login again."
