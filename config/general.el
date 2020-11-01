;; General configuration - much taken from prelude

(install-package-list
 '(browse-kill-ring
   diff-hl
   editorconfig
   hl-todo
   undo-tree
   smartparens
   zop-to-char
   exec-path-from-shell
   super-save
   direx
   popwin
   which-key
   volatile-highlights
   anzu
   bm
   crux))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; more useful frame title, that shows either a file or a buffer name
;; (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; show available keybindings after you start typing
(require 'which-key)
(which-key-mode +1)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; smart pairing for all
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(show-smartparens-global-mode +1)

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; Go to last place in any buffer
(save-place-mode 1)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60)
(savehist-mode +1)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(require 'super-save)
(super-save-mode +1)

;; highlight the current line
(global-hl-line-mode +1)

;; temporarily highlight text affected by certain operations (e.g. yank, undo)
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(defun enable-flyspell ()
  "Enable command `flyspell-mode' if spellcheck program can be found."
  (when (executable-find ispell-program-name)
    (flyspell-mode +1)))

(defun enable-whitespace ()
  "Keep the whitespace decent all the time (in this buffer)."
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)
  (whitespace-mode +1))

(add-hook 'text-mode-hook 'enable-flyspell)
(add-hook 'text-mode-hook 'enable-whitespace)

;; bookmarks
(require 'bookmark)
(setq bookmark-save-flag 1)

;; anzu-mode enhances isearch & query-replace by showing total matches and current match position
(require 'anzu)
(global-anzu-mode)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; sensible undo
(global-undo-tree-mode)

;; diff-hl - diff highlighting
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; use settings from .editorconfig file when present
(require 'editorconfig)
(editorconfig-mode 1)

;; Disable scroll acceleration
(setq ns-use-mwheel-momentum nil)

;; No scroll bar
(scroll-bar-mode -1)

;; Active follow mouse
(setq mouse-autoselect-window t)

;; save desktop when idle
(run-with-idle-timer 300 t 'desktop-save-in-desktop-dir)

(require 'popwin)
(popwin-mode 1)

(require 'direx)
(require 'direx-project)
(push '(direx:direx-mode :position left :width 75 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x C-k") 'direx-project:jump-to-project-root-other-window)
(global-set-key (kbd "C-x C-j") 'direx:find-directory-other-window)


;; BOOKMARKS

(require 'bm)

;; Load bookmarks when starting emacs.
(setq bm-restore-repository-on-load t)

;; Key-bindings
(global-set-key (kbd "<C-f9>") 'bm-toggle)
(global-set-key (kbd "<f9>")   'bm-next)
(global-set-key (kbd "<f10>")  'bm-previous)


;; KEY BINDINGS

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; replace zap-to-char functionality with the more powerful zop-to-char
(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; Comments
(global-set-key [f3] 'comment-region)
(global-set-key [f4] 'uncomment-region)

;; Buffer revert
(global-set-key [f5] 'revert-buffer)

;; Goto line
(global-set-key [f7] 'goto-line)

;; Toggle buffer
(global-set-key [C-tab] '(lambda ()
                           (interactive)
                           (switch-to-buffer (other-buffer))))

;; Rebind delete key to delete-char function
(global-set-key [delete] 'delete-char)

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; Handy key definition
(global-set-key (kbd "M-p") 'unfill-region)

;; Toggle first-character and first-non-whitespace-character with C-a
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
