# Ben's `.emacs.d`

Uses some stuff has been taken from [Prelude](https://github.com/bbatsov/prelude)

Most attention has been given to:

* Python
* Clojure
* Web (html/css/js)
* Running Emacs in a terminal if needed

![(Darth Vader) I see you constructed a new .emacs.d. Your skills are complete.](new-emacsd.jpg)


## Installation

Run: `./install.sh`


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


## Updating Packages

1. `M-x straight-thaw-versions`
2. `M-x straight-pull-recipe-repositories`
3. `M-x straight-pull-all`
4. `M-x straight-freeze-versions`
5. Commit the updated `straight/versions/default.el`

## Cheat Sheet

### General

```
M-y - Browse kill ring
```

### Helm

```
C-o - Cycle through candidate sections
RET - Default action on candidate
TAB - See actions for candidate
C-SPC - Mark candidate

C-x C-f - File search
  M-BACKSPACE - Navigate up dirs
  C-s - Grep within file/dir
    TAB - Select option to save search results to buffer
C-c h o - String match current file
C-c h r - Regex match current file
C-c p - Search for files/buffers in and switch between projectile project

C-c h b - Resume previous Helm session

```
