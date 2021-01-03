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


;; OCTAVE

(use-package octave
  :bind (("C-c C-j" . octave-send-line)
         ("C-c C-k" . octave-send-defun)))
