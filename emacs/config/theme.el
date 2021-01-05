(use-package zenburn-theme
  :straight t
  :config
  (custom-theme-set-faces
   'zenburn
   '(font-lock-comment-face ((t (:foreground "#7f9f7f" :slant italic))))
   '(mode-line-inactive ((t (:foreground "#5F7F5F"
                             :background "#303030"
                             :box (:line-width -1 :style released-button)))))
   '(flyspell-duplicate ((t (:foreground "peachpuff1" :underline nil :weight normal :slant italic))))
   '(flyspell-incorrect ((t (:foreground "rosybrown1" :underline nil :weight normal :slant italic))))))
