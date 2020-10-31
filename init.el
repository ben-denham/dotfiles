;; TODO:
;; 1. https://realpython.com/emacs-the-best-python-editor/
;; 2. Learn how to use imenu + imenu-anywhere? (set-default 'imenu-auto-rescan t)
;; 3. Flycheck?
;; 4. Company?
;; 5. lang-server?

(defvar emacs-dir (file-name-directory load-file-name)
  "The root dir of the Emacs configuration.")

(desktop-save-mode 1)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" emacs-dir))

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end))))


;; INSTALLING PACKAGES

(require 'package)

;; accessing a package repo over https on Windows is a no go, so we
;; fallback to http there
(if (eq system-type 'windows-nt)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(defun install-package-list (package-list)
  (mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
        package-list))


;; GENERAL PACKAGES

(install-package-list
 '(zenburn-theme
   magit
   gitconfig-mode
   gitignore-mode
   dockerfile-mode))

(require 'zenburn-theme)

;; CONFIGURATION SECTIONS

(defvar config-dir (expand-file-name "config" emacs-dir))

(load (expand-file-name "general.el" config-dir))
(load (expand-file-name "ido.el" config-dir))
(load (expand-file-name "programming.el" config-dir))
(load (expand-file-name "markup.el" config-dir))
(load (expand-file-name "web.el" config-dir))
(load (expand-file-name "php.el" config-dir))
(load (expand-file-name "lisp.el" config-dir))
(load (expand-file-name "python.el" config-dir))
(load (expand-file-name "music.el" config-dir))
