(install-package-list
 '(web-mode
   js2-mode
   css-mode
   scss-mode))

;; Web

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist
'("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 4)
(setq web-mode-markup-indent-offset 2)

;; make web-mode play nice with smartparens
(setq web-mode-enable-auto-pairing nil)
(sp-with-modes '(web-mode)
  (sp-local-pair "%" "%"
                 :unless '(sp-in-string-p)
                 :post-handlers '(((lambda (&rest _ignored)
                                     (just-one-space)
                                     (save-excursion (insert " ")))
                                   "SPC" "=" "#")))
  (sp-local-tag "%" "<% "  " %>")
  (sp-local-tag "=" "<%= " " %>")
  (sp-local-tag "#" "<%# " " %>"))

;; JS

(setq js-indent-level 2)
(setq js2-bounce-indent-p nil)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;; ES6/JSX (https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea)
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))


(with-eval-after-load 'js2-mode
  (add-hook 'js2-mode-hook
            (lambda ()
              ;; electric-layout-mode doesn't play nice with smartparens
              (setq-local electric-layout-rules '((?\; . after)))
              (setq mode-name "JS2")
              (js2-imenu-extras-mode +1))))

;; CSS

(defun base-css-mode-hook ()
  (run-hooks 'my-prog-mode-hook))

(with-eval-after-load 'css-mode
  (setq css-indent-offset 4)
  (add-hook 'css-mode-hook css-mode-hook))

;; SCSS

(with-eval-after-load 'css-mode
  ;; turn off annoying auto-compile on save
  (setq scss-compile-at-save nil)
  (add-hook 'scss-mode-hook base-css-mode-hook))
