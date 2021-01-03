;; Major language modes

(use-package hy-mode
  :straight t)

(use-package clojure-mode
  :straight t)

;; (defun spit-scad-last-expression ()
;;   "Spit open-scad code from cider repl into repl.scad."
;;   (interactive)
;;   (cider-interactive-eval
;;    (format
;;     "(require 'scad-clj.scad)
;;      (spit \"repl.scad\"
;;            (scad-clj.scad/write-scad %s))"
;;     (cider-last-sexp))))

(use-package cider
  :straight t
  :custom
  (nrepl-log-messages t)
  :bind (:map cider-mode-map
         ;; ("C-x C-g" . spit-scad-last-expression)
         ("C-c C-w" . cider-eval-buffer))
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              ;; interactive modes don't need whitespace checks
              (whitespace-mode -1))))

(defun my-elisp ()
  (eldoc-mode +1)
  (setq mode-name "EL"))
(setq my-elisp-hook 'my-elisp)
(add-hook 'emacs-lisp-mode-hook (lambda () (run-hooks 'my-elisp-hook)))
(add-hook 'ielm-mode-hook (lambda () (run-hooks 'my-elisp-hook)))


;; Extra minor modes

(use-package smartparens
  :straight t
  :hook ((clojure-mode . smartparens-strict-mode)
         (cider-mode . smartparens-strict-mode)
         (emacs-lisp-mode . smartparens-strict-mode)
         (ielm-mode . smartparens-strict-mode)
         (hy-mode . smartparens-strict-mode)))

(use-package rainbow-delimiters
  :straight t
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (cider-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)
         (ielm-mode . rainbow-delimiters-mode)
         (hy-mode . rainbow-delimiters-mode)))
