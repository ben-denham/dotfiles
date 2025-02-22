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

(use-package ellama
  :ensure t
  :bind ("C-c e" . ellama-transient-main-menu)
  :init
  ;; setup key bindings
  ;; (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  ;; (setopt ellama-language "German")
  ;; could be llm-openai for example
  (require 'llm-openai)
  (setopt ellama-provider
          (make-llm-openai-compatible
           :url "http://localhost:9000"))
  ;; customize display buffer behaviour
  ;; see ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  ;; send last message in chat buffer with C-c C-c
  (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message))
