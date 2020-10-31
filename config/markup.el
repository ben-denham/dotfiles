(install-package-list
 '(json-mode
   yaml-mode
   auctex
   markdown-mode
   flymd))

;; XML

(require 'nxml-mode)
(push '("<\\?xml" . nxml-mode) magic-mode-alist)

;; pom files should be treated as xml files
(add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode))

(setq nxml-child-indent 4)
(setq nxml-attribute-indent 4)
;; Adds XML declaration to beginning of new files.
(setq nxml-auto-insert-xml-declaration-flag nil)
;; Auto-completes from "</"
(setq nxml-slash-auto-complete-flag t)


;; YAML

;; yaml-mode doesn't derive from prog-mode, but we can at least enable
;; whitespace-mode, subword-mode, and apply cleanup.
(add-hook 'yaml-mode-hook 'whitespace-mode)
(add-hook 'yaml-mode-hook 'subword-mode)
(add-hook 'yaml-mode-hook
          (lambda () (add-hook 'before-save-hook 'whitespace-cleanup nil t)))


;; LaTeX
;; TODO: Consider cdlatex for fast-math-entry

;; AUCTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-close-quote "")
(setq TeX-open-quote "")
(setq-default TeX-master nil)
;; use pdflatex
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (abbrev-mode +1)
                             (smartparens-mode +1)
                             (LaTeX-math-mode 1)))


;; MARKDOWN

(require 'flymd)
;; See: https://github.com/mola-T/flymd/blob/master/browser.md
(defun my-flymd-browser-function (url)
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url url)))
(setq flymd-browser-open-function 'my-flymd-browser-function)
