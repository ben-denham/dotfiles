## Ben's `tmux`

Configuration for `tmux` and `tmuxinator`.


## Installation

Run: `./install.sh`


## Common Tmux Keys

https://www.hamvocke.com/blog/a-quick-and-easy-guide-to-tmux/

* `M-l + ?` - list all available keys
* `M-l` - prefix key
  * `M-d` or `d` - detach
  * `r` - reload config
  * `:` - enter command prompt
  * `I` - install added tmux plugins
* Pane splitting
  * `M-J` - split horizontally
  * `M-K` - split vertically
* Emacs-style pane navigation
  * `M-z` - zoom/unzoom pane
  * `M-o` - cycle through panes
  * `M-B` - move left
  * `M-F` - move right
  * `M-P` - move up
  * `M-N` - move down
* Window navigation
  * `M-+` - new window
  * `M-Shift-left` - next window
  * `M-Shift-right` - previous window
  * `M-[0-9]` - switch to window
* Session navigation
  * `M-l + Space` - Open session chooser
* Subcommands
  * `tmux ls` - list active sessions
  * `tmux kill-server` - kill all tmux sessions
* Copy mode (scrolling back through buffer)
  * `q` - exit copy mode / return to prompt
