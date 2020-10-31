(install-package-list
 '(guru-mode
   rainbow-mode))

(setq c-basic-offset 2)
(setq cperl-indent-level 2)

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; guru-mode should only warn
(setq guru-warn-only t)

(defun my-prog-mode ()
  ;; Treat camel/pascal names as multiple words.
  (subword-mode +1)
  ;; Enable spell-checking
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode))
  ;; Guru-mode encourages good emacs practice.
  (guru-mode +1)
  ;; Hex colours highlighted.
  (rainbow-mode +1)
  (smartparens-mode +1)
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t)))
  (enable-whitespace))

;; Most programming languages extend prog-mode
(setq my-prog-mode-hook 'my-prog-mode)
(add-hook 'prog-mode-hook (lambda () (run-hooks 'my-prog-mode-hook)))


;; OCTAVE

(require 'octave)
(define-key octave-mode-map
  (kbd "C-c C-j") 'octave-send-line)
(define-key octave-mode-map
  (kbd "C-c C-k") 'octave-send-defun)
