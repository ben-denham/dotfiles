;; Copy pasted from ruby-mode.el
(defun python--encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun python--detect-encoding ()
  (let ((coding-system
         (or save-buffer-coding-system
             buffer-file-coding-system)))
    (if coding-system
        (symbol-name
         (or (coding-system-get coding-system 'mime-charset)
             (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun python--insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

(defun python-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (python--encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (python--detect-encoding)))
        (when coding-system
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
                 ;; update existing encoding comment if necessary
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (python--insert-coding-comment coding-system)))
          (when (buffer-modified-p)
            (basic-save-buffer-1)))))))

(add-hook 'python-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (add-hook 'after-save-hook 'python-mode-set-encoding nil 'local)))


(use-package ein
  :straight t)


(use-package jupyter
  :straight t
  :config
  ;; Don't display these buffers when output is added to them, since we
  ;; will be viewing rich output in the browser console session
  (setq custom-jupyter-quiet-buffers '("*jupyter-display*" "*jupyter-output*" "*jupyter-traceback*"  "*jupyter-result*" "*jupyter-error*"))
  (when (not (boundp 'orig-jupyter-display-current-buffer-reuse-window))
   (setq orig-jupyter-display-current-buffer-reuse-window (symbol-function 'jupyter-display-current-buffer-reuse-window)))
  (defun jupyter-display-current-buffer-reuse-window (&optional msg-type alist &rest actions)
    (when (not (member (buffer-name) custom-jupyter-quiet-buffers))
      (apply orig-jupyter-display-current-buffer-reuse-window msg-type alist actions)))
  ;; Add custom-jupyter-eval-sentence for evaluating contiguous blocks of code
  (defun custom-jupyter-eval-sentence ()
    (interactive)
    (when-let* ((bounds (bounds-of-thing-at-point 'sentence)))
      (cl-destructuring-bind (beg . end) bounds
        (jupyter-eval-region beg end))))
  (define-key jupyter-repl-interaction-mode-map (kbd "C-c C-c") #'custom-jupyter-eval-sentence))
