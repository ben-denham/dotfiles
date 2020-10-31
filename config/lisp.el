(install-package-list
 '(rainbow-delimiters
   clojure-mode
   cider
   hy-mode))

(defun my-lisp ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq my-lisp-hook 'my-lisp)

;; CLOJURE

(defun spit-scad-last-expression ()
  "Spit open-scad code from cider repl into repl.scad."
  (interactive)
  (cider-interactive-eval
   (format
    "(require 'scad-clj.scad)
     (spit \"repl.scad\"
           (scad-clj.scad/write-scad %s))"
    (cider-last-sexp))))

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook (lambda ()
                                 (run-hooks 'my-lisp-hook))))

(with-eval-after-load 'cider
  (define-key cider-mode-map
    (kbd "C-x C-g") 'spit-scad-last-expression)
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (run-hooks 'my-lisp-hook)
              ;; interactive modes don't need whitespace checks
              (whitespace-mode -1))))


;; EMACS LISP

(defun my-elisp ()
  (run-hooks 'my-lisp-hook)
  (eldoc-mode +1)
  (setq mode-name "EL"))
(setq my-elisp-hook 'my-elisp)

(add-hook 'emacs-lisp-mode-hook (lambda () (run-hooks 'my-elisp-hook)))
(add-hook 'ielm-mode-hook (lambda () (run-hooks 'my-elisp-hook)))


;; HY

(add-hook 'hy-mode-hook (lambda () (run-hooks 'my-lisp-hook)))
