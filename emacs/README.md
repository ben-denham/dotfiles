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

### LSP Dependencies

#### Python

`pip install python-lsp-server[pylint]==1.4.1 pylsp-mypy==0.5.7`


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

### `lsp-mode`

```
C-c l - Keymap prefix
C-c l r r - Rename the symbol (and all references to it).

M-. - Peek definition of the identifier at point.
M-; - Peek references to the identifier at point.
M-, - Go back from peek

C-: - Describe identifier at point (see signature and docstring)
M-/ - Start auto-completion

C-c e l - List flycheck errors
C-M-; - List symbols in current workspace (not implemented for pylsp)
C-M-. - List symbols in current workspace (not implemented for pylsp)
C-c l g r - List references to the symbol at point in separate buffer.

C-c f c - Origami - close node
C-c f C - Origami - close all nodes
C-c f o - Origami - open node recursively
C-c f O - Origami - open all nodes
C-c f u - Origami - undo
C-c f r - Origami - redo
```

#### Custom LSP servers

`lsp-mode` is configured to launch the Python `pylsp` server with
`~/.emacs.d/pylsp.sh`, which will log to `~/.emacs.d/pylsp-sh.log` and
search for directory-specific pylsp commands in
`~/.emacs.d/dir-to-pylsp.txt`, which is formatted like:

```
/home/ben/phd/myproject make pylsp
```

If you need to change the mapping between the paths as seen by emacs
and your LSP server (e.g. because your server is run in a Docker
container with directories mounted in different locations), you can
provide custom path mappings in a `.dir-locals.el`:

```
((nil . ((custom-lsp-path-mappings . (("" . "/home/jovyan/work"))))))
```

#### Switch Python Linter

E.g. in `.dir-locals.el`:

```
((nil . ((lsp-pylsp-plugins-flake8-enabled . t)
         (lsp-pylsp-plugins-pylint-enabled . nil))))
```
