(setq c-basic-offset 2)
(setq cperl-indent-level 2)

;; Most programming languages extend prog-mode
(defun my-prog-mode ()
  ;; Treat camel/pascal names as multiple words.
  (subword-mode +1)
  ;; Enable spell-checking
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode))
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t)))
  (enable-whitespace))
(setq my-prog-mode-hook 'my-prog-mode)
(add-hook 'prog-mode-hook (lambda () (run-hooks 'my-prog-mode-hook)))

;; Guru-mode encourages good emacs practice.
(use-package guru-mode
  :straight t
  :custom
  (guru-warn-only t "guru-mode should only warn")
  :hook ((prog-mode . guru-mode)))

;; Hex colours highlighted.
(use-package rainbow-mode
  :straight t
  :hook ((prog-mode . rainbow-mode)))

(use-package smartparens
  :straight t
  :hook ((prog-mode . smartparens-mode)))

;; show the name of the current function definition in the modeline
(use-package which-func
  :config
  (which-function-mode 1))


;; LSP and other IDE functionality

(use-package lsp-mode
  :straight t
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (python-mode . lsp-deferred))
  :bind (("C-:" . lsp-describe-thing-at-point))
  :init
  ;; Disable some features
  (setq lsp-enable-snippet nil
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil)
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-pylsp-server-command (concat emacs-dir "pylsp.sh"))
  ;; Python linting configuration
  (setq lsp-pylsp-configuration-sources ["pylint" "flake8"]
        lsp-pylsp-plugins-pycodestyle-enabled nil
        lsp-pylsp-plugins-pydocstyle-enabled nil
        lsp-pylsp-plugins-flake8-enabled nil
        lsp-pylsp-plugins-pylint-enabled t))

(load (expand-file-name "custom-lsp-clients.el" config-dir))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :bind (("M-." . lsp-ui-peek-find-definitions)
         ("M-;" . lsp-ui-peek-find-references)
         ("C-M-;" . lsp-ui-imenu))
  :config
  (setq lsp-modeline-diagnostics-scope :workspace
        lsp-ui-doc-enable nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-signature-render-documentation nil))

;; Navigation
(use-package helm-lsp
  :straight t
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

;; Linting
(defun set-flycheck-margins ()
  (setq right-margin-width 1)
  (flycheck-refresh-fringes-and-margins))
(use-package flycheck
  :straight t
  :config
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c e"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
    flycheck-command-map)
  (global-flycheck-mode)
  (setq-default flycheck-highlighting-mode nil)
  (setq-default flycheck-indication-mode 'right-margin)
  (add-hook 'flycheck-mode-hook #'set-flycheck-margins))

;; Auto-complete
(use-package company
  :straight t
  :bind (("M-/" . company-complete))
  :config
  (global-company-mode)
  ;; Disable auto-start
  (setq company-idle-delay nil))

;; Code-folding
(use-package origami
  :straight t
  :bind (("C-c f o" . origami-open-node-recursively)
         ("C-c f c" . origami-close-node)
         ("C-c f O" . origami-open-all-nodes)
         ("C-c f C" . origami-close-all-nodes)
         ("C-c f u" . origami-undo)
         ("C-c f r" . origami-redo))
  :config
  (global-origami-mode))
(use-package lsp-origami
  :straight t
  :config
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

;; (use-package dap-mode
;;   :straight t)
;; (use-package dap-python)


;; OCTAVE

(use-package octave
  :bind (("C-c C-j" . octave-send-line)
         ("C-c C-k" . octave-send-defun)))

;; SCALA

(use-package scala-mode
  :straight t
  :interpreter ("scala" . scala-mode))
