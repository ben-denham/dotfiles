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

(enable-theme 'zenburn)

(setq transparency-enabled nil)
(defun toggle-transparency ()
  (interactive)
  (if transparency-enabled
      (progn
        (set-background-color "#3f3f3f")
        (set-face-background 'hl-line "#383838")
        (setq transparency-enabled nil))
    (progn
      (set-background-color "ARGBBB000000")
      (set-face-background 'hl-line "ARGBBB000000")
      (setq transparency-enabled t))))
(global-set-key (kbd "C-c C-t") 'toggle-transparency)
