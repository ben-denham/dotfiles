#!/bin/bash

echo "=== konsole configuration ==="

sudo apt install konsole

KONSOLE_CONFIG_DIR=~/.local/share/konsole
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

if [ -h $KONSOLE_CONFIG_DIR ]; then
    # Remove if existing symlink
    rm $KONSOLE_CONFIG_DIR
elif [ -d $KONSOLE_CONFIG_DIR ]; then
    # Backup if existing directory
    mv $KONSOLE_CONFIG_DIR "${KONSOLE_CONFIG_DIR}.bak"
fi

ln -s "$SCRIPT_DIR/konsole" $KONSOLE_CONFIG_DIR

# Install Powerline and emoji-compatible Fonts
mkdir -p ~/.local/share/fonts
cd ~/.local/share/fonts && curl -fLo "Hack Regular Nerd Font Complete.ttf" https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/Hack/Regular/complete/Hack%20Regular%20Nerd%20Font%20Complete.ttf

mkdir /tmp/noto
cd /tmp/noto
wget https://noto-website-2.storage.googleapis.com/pkgs/Noto-hinted.zip
unzip Noto-hinted.zip
cp *otf *otc ~/.local/share/fonts
rm -r /tmp/noto

mkdir -p ~/.config/fontconfig
ln -s "$SCRIPT_DIR/fonts.conf" ~/.config/fontconfig/fonts.conf

sudo fc-cache -f -v
