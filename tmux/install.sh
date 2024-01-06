#!/bin/bash

echo "=== tmux configuration ==="

sudo apt install tmux tmuxinator xsel

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


# .tmux.conf

if [ -h ~/.tmux.conf ]; then
    # Remove if existing symlink
    rm ~/.tmux.conf
elif [ -f ~/.tmux.conf ]; then
    # Backup if existing file
    mv ~/.tmux.conf ~/.tmux.conf.bak
fi

ln -s "$SCRIPT_DIR/.tmux.conf" ~/.tmux.conf


# .tmuxinator

if [ -h ~/.tmuxinator ]; then
    # Remove if existing symlink
    rm ~/.tmuxinator
elif [ -d ~/.tmuxinator ]; then
    # Backup if existing directory
    mv ~/.tmuxinator ~/.tmuxinator.bak
fi

ln -s "$SCRIPT_DIR/.tmuxinator" ~/.tmuxinator

# Install tmux plugins
~/dotfiles/tmux/tpm/scripts/install_plugins.sh
