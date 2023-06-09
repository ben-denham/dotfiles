(defun timeclocking--goto-newline-at-end ()
  "Goes to the end of the buffer, and inserts a newline if there isn't already one there."
  (end-of-buffer)
  (let ((last-character-str (buffer-substring-no-properties (point-max) (- (point-max) 1))))
    (unless (string= last-character-str "\n")
      (insert "\n"))))

(defun timeclocking--last-entry-type ()
  "Returns the type character (?i or ?o) of the last timeclocking entry. Returns nil if there are no entries."
  ;; We'll be moving the point around, so restore the point after.
  (save-excursion
    (end-of-buffer)
    ;; Search backwards for the last line starting with "i" or "o"
    (re-search-backward "^[io]" nil t)
    ;; Return character after point
    (char-after)))

(defun timeclocking-out (&optional time)
  "Safely insert an o entry at the end of the current buffer."
  (interactive)
  (or time (setq time (current-time)))
  (timeclocking--goto-newline-at-end)
  (if (equal (timeclocking--last-entry-type) ?i)
      (insert (concat "o " (format-time-string "%F %R" time)))
    (message "No previous i entry to clock out from")))

(defun timeclocking-in (&optional time label comment)
  "Safely insert an i entry at the end of the current buffer."
  (interactive)
  (or time (setq time (current-time)))
  ;; Prompt for a label until one is specified
  (while (or (not label) (string= label ""))
    (setq label (read-string "Label: " nil t)))
  (or comment (setq comment (read-string "Comment: " nil t)))
  ;; When a comment is provided, prefix it with double-space
  (unless (string= comment "")
    (setq comment (concat "  " comment)))
  ;; When the last entry is an i entry, timeclock out first.
  (when (equal (timeclocking--last-entry-type) ?i)
    (timeclocking-out time))
  (timeclocking--goto-newline-at-end)
  (insert (concat "i " (format-time-string "%F %R" time) " " label comment)))

(defvar timeclocking-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i") 'timeclocking-in)
    (define-key map (kbd "C-c o") 'timeclocking-out)
    map))

(defvar timeclocking-highlights
  '(("^i" . 'font-lock-constant-face)
    ("^o" . 'font-lock-string-face)
    ("  .*" . 'font-lock-variable-name-face)
    ("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]" . 'font-lock-function-name-face)))

(define-derived-mode timeclocking-mode fundamental-mode "Timeclocking"
  "Major mode for timeclock files."
  (setq-local comment-start ";")
  (setq-local font-lock-defaults '(timeclocking-highlights))
  ;; Define ";" as the comment character, and "\n" to end a comment
  (modify-syntax-entry ?\; "< b")
  (modify-syntax-entry ?\n "> b"))

(add-to-list 'auto-mode-alist '("\\.timelog" . timeclocking-mode))
