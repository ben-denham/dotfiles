;; Append term-keys' konsole-keytab configuration to the keytab file.

(load (expand-file-name "package-manager-setup.el" (file-name-directory load-file-name)))
(load (expand-file-name "config/terminal.el" (file-name-directory load-file-name)))
(require 'term-keys-konsole)

(with-temp-buffer
  (insert (term-keys/konsole-keytab))
  (append-to-file (point-min) (point-max) "~/.local/share/konsole/Emacs.keytab"))
