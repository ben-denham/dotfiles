(use-package php-mode
  :straight t)

(use-package drupal-mode
  :straight t)

(use-package geben
  :straight t)

(use-package phpcbf
  :straight t
  :bind ((:map php-mode-map
          ("C-x C-;" . phpcbf)))
  :config
  ;; composer global require "squizlabs/php_codesniffer=*"
  ;; composer global require drupal/coder
  ;; phpcs --config-set installed_paths ~/.composer/vendor/drupal/coder/coder_sniffer/Drupal
  (custom-set-variables
   '(phpcbf-executable "~/.composer/vendor/bin/phpcbf")
   '(phpcbf-standard "PSR2"))
  ;;(add-hook 'php-mode-hook 'phpcbf-enable-on-save)
  )

(load (expand-file-name "php-doc.el" config-dir))
