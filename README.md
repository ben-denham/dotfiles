# Ben's `.emacs.d`

Uses some stuff has been taken from [Prelude](https://github.com/bbatsov/prelude)

Most attention has been given to:

* Python
* Clojure
* Web (html/css/js)
* Running Emacs in a terminal if needed

![(Darth Vader) I see you constructed a new .emacs.d. Your skills are complete.](new-emacsd.jpg)


## Running Emacs in a Terminal

Ubuntu's default Gnome Terminal does not have adequate support for
passing certain modifier key combinations through to Emacs, so you
should instead use `Konsole` (another modern terminal emulator) and
follow the appropriate instructions for configuring `term-keys`:
https://github.com/CyberShadow/term-keys#konsole

You can then run Emacs in the terminal with: `emacs -nw`

You may want to make `Konsole` your default terminal in Ubuntu:

```
sudo update-alternatives --config x-terminal-emulator
```

You may want to set the following settings in `Konsole`:

* Enable `Copy on select`

## TODO

* tmux + tmuxinator
  * tmux conf
  * tpm plugins
* https://realpython.com/emacs-the-best-python-editor/
* Flycheck?
* Company?
* Navigation
  * Learn how to use imenu + imenu-anywhere? (set-default 'imenu-auto-rescan t)
  * lang-server?
  * gtags?
