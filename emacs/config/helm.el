;; Helm is like ido-mode, but lists options vertically instead of
;; horizontally in the mini-buffer.
(use-package helm
  :straight t
  ;; Based on http://tuhdo.github.io/helm-intro.html
  :config
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x b") #'helm-mini)
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  ;; persistent-action doesn't quit the helm session
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (helm-mode 1))

(use-package helm-projectile
  :straight t
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (global-set-key (kbd "C-c p") 'helm-projectile))
