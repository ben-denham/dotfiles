;; You need to run M-x copilot-install-server and M-x copilot-login
;; Logout with: M-x copilot-logout
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)

(use-package shell-maker
  :straight (:host github :repo "xenodium/shell-maker" :files ("shell-maker.el")))
;; Logout by deleting ~/.config/copilot-chat and ~/.cache/copilot-chat
(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode))

(use-package gptel
  :ensure t
  :init
  (setq gptel-model 'coder)
  (setq gptel-backend (gptel-make-openai "llama-cpp"
                        :stream t
                        :protocol "http"
                        :host "localhost:9000"
                        :models '(coder)))
  ; NOTE: This is not GitHub Copilot, just GitHub Models:
  ;; (setq gptel-model 'gpt-4o)
  ;; (setq gptel-backend (gptel-make-openai "GitHub Models"
  ;;                       :host "models.inference.ai.azure.com"
  ;;                       :endpoint "/chat/completions?api-version=2024-05-01-preview"
  ;;                       :stream t
  ;;                       :key "TODO"
  ;;                       :models '(gpt-4o)))
  )

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
         :url "http://localhost:9000/v1/"))
  ;; customize display buffer behaviour
  ;; see ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  (setq llm-warn-on-nonfree nil)
  :config
  ;; send last message in chat buffer with C-c C-c
  (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message))
