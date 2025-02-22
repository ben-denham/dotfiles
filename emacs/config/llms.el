(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :config
  (setq aidermacs-default-model "openai/coder")
  (setq aidermacs-architect-model "openai/coder")
  (setq aidermacs-editor-model "openai/coder")
  (setenv "OPENAI_API_BASE" "http://localhost:9000")
  (setenv "OPENAI_API_KEY" "NONE")
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
  (setq aidermacs-auto-commits nil)
  (setq aidermacs-use-architect-mode nil))
