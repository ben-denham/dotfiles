;; Configuration for running emacs in a terminal (specifically:
;; Konsole).

;; NOTE: After changing key mapping configuration, you must update
;; Konsole's keytab by running
;; ~/dotfiles/konsole/update-emacs-keytab.sh, or by following the
;; instructions at:
;; https://github.com/CyberShadow/term-keys/blob/master/README.md#konsole


;; Konsole doesn't recognise e.g. 5+Shift+Ctrl as C-%, but will accept
;; %+Shift+Ctrl, so we must add explicit key mappings for shifted
;; special symbols.
(setq konsole-extra-shift-keys
      '("~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_"
        "+" "{" "}" "|"
        ":" "\""
        "<" ">" "?"))

(defun konsole-extra-want-key-p (key mods)
  ;; NOTE: key names come from the second column of the
  ;; term-keys/mapping table:
  ;; https://github.com/CyberShadow/term-keys/blob/master/term-keys.el#L50
  (let ((shift (elt mods 0))
        (ctrl (elt mods 1))
        (meta (elt mods 2))
        (super (elt mods 3))
        (hyper (elt mods 4))
        (alt (elt mods 5)))
    (and
     ;; Exclude certain keys that work fine, and that we use for
     ;; shortcuts in the terminal or tmux.
     (not (member key '("slash" "space" "+" "Up" "Down" "Left" "Right")))
     ;; Exclude shift-only special characters (otherwise
     ;; e.g. shift+backspace will have a special code).
     (not (and shift (not (or ctrl meta super hyper alt))))
     (or
      (and
       ;; We don't need Super/Hyper/Alt modifiers
       (not super)
       (not hyper)
       (not alt)
       ;; Add shifted character keys for when shift is pressed and at
       ;; least one other modifier (don't want replacements for basic
       ;; characters).
       shift
       (or ctrl meta)
       (member key konsole-extra-shift-keys))
      (term-keys/want-key-p-def key mods)))))

(use-package term-keys
  :straight (:host github :repo "CyberShadow/term-keys")
  :custom
  (term-keys/want-key-p-func 'konsole-extra-want-key-p)
  :config
  (term-keys-mode +1)
  (setq term-keys/mapping
        (append term-keys/mapping
                (mapcar (lambda (key)
                          ;; This vector has enough key map information
                          ;; for configuring Konsole.
                          (vector key key nil key nil))
                        konsole-extra-shift-keys))))

;; Enable use of mouse inside terminal.
(xterm-mouse-mode +1)

;; Enable copy/paste with X Clipboard
(use-package xclip
  :straight t
  :config
  (xclip-mode +1))
;; Prevent double pasting inside tmux.
(global-unset-key (kbd "<mouse-2>"))
;; Mouse-select in terminal emacs should also copy to primary
;; selection.
(setq mouse-drag-copy-region t)
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard nil)
