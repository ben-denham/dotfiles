(use-package nxml-mode
  :custom
  (nxml-child-indent 4)
  (nxml-attribute-indent 4)
  (nxml-auto-insert-xml-declaration-flag nil "Adds XML declaration to beginning of new files")
  (nxml-slash-auto-complete-flag t "Auto-completes from \"</\"")
  ;; pom files should be treated as xml files
  :mode ("\\.pom$" . nxml-mode)
  :magic ("<\\?xml" . nxml-mode))

(use-package json-mode
  :straight t)

(use-package yaml-mode
  :straight t
  ;; yaml-mode doesn't derive from prog-mode, but we can at least
  ;; enable whitespace-mode, subword-mode, and apply cleanup.
  :hook
  ((yaml-mode . whitespace-mode)
   (yaml-mode . subword-mode)
   (yaml-mode . (lambda ()
                  (add-hook 'before-save-hook 'whitespace-cleanup nil t)))))

(use-package markdown-mode
  :straight t
  :custom
  (markdown-link-make-text-function (lambda (uri) uri))
  (markdown-disable-tooltip-prompt t))

(use-package mermaid-mode
  :straight t)

(use-package flymd
  :straight t
  :custom
  ;;
  (flymd-browser-open-function
   (lambda (url)
     (let ((browse-url-browser-function 'browse-url-firefox))
       (browse-url url)))
   "See: https://github.com/mola-T/flymd/blob/master/browser.md"))

(use-package auctex
  :straight t
  :hook ((LaTeX-mode . abbrev-mode)
         ;;(LaTeX-mode . smartparens-mode)
         (LaTeX-mode . LaTeX-math-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-close-quote "")
  (TeX-open-quote "")
  (TeX-PDF-mode t "use pdflatex")
  :config
  (setq-default TeX-master nil))

;; TODO: Consider cdlatex for fast-math-entry
