(install-package-list
 '(php-mode
   phpcbf
   drupal-mode
   geben))

(require 'drupal-mode)

(require 'phpcbf)
;; composer global require "squizlabs/php_codesniffer=*"
;; composer global require drupal/coder
;; phpcs --config-set installed_paths ~/.composer/vendor/drupal/coder/coder_sniffer/Drupal
(custom-set-variables
 '(phpcbf-executable "~/.composer/vendor/bin/phpcbf")
 '(phpcbf-standard "PSR2"))
(define-key php-mode-map (kbd "C-x C-;") 'phpcbf)
;;(add-hook 'php-mode-hook 'phpcbf-enable-on-save)

(load (expand-file-name "php-doc.el" config-dir))
