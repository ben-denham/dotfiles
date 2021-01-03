(use-package ido
  :straight t
  :custom
  (ido-enable-prefix nil)
  (ido-enable-flex-matching t)
  (ido-create-new-buffer 'always)
  (ido-use-filename-at-point 'guess)
  (ido-max-prospects 10)
  (ido-save-directory-list-file (expand-file-name "ido.hist" emacs-dir))
  (ido-default-file-method 'selected-window)
  (ido-auto-merge-work-directories-length -1)
  :config
  (ido-mode +1)
  (ido-everywhere +1))

;; Sublime-style fuzzy-matching.
(use-package flx-ido
  :straight t
  :custom
  (ido-use-faces nil "Disable ido faces to see flx highlights")
  :config
  (flx-ido-mode +1))

;; Replaces standard completing with ido completion (formerly ido-ubiquitous)
(use-package ido-completing-read+
  :straight t
  :config
  (ido-ubiquitous-mode +1))

;; ido for M-x
(use-package smex
  :straight t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :custom
  (smex-save-file (expand-file-name ".smex-items" emacs-dir) "Remember recently and most frequently used commands")
  :config
  (smex-initialize))
