(require 'lsp-mode)

(defcustom custom-lsp-path-mappings '()
  "List of dotted pairs of (host-path . remote-server-path)")
(put 'custom-lsp-path-mappings 'safe-local-variable
     (lambda (path-mappings)
       (seq-every-p
        (lambda (path-mapping)
          (and
           (stringp (car path-mapping))
           (stringp (cdr path-mapping))))
        path-mappings)))

(defun custom-lsp--uri->path (uri)
  (let ((path (lsp--uri-to-path-1 uri)))
    (-if-let ((local . remote) (-first (-lambda ((_ . remote-path))
                                         (s-contains? remote-path path))
                                       custom-lsp-path-mappings))
        (concat (lsp-workspace-root) (s-replace remote local path))
      path)))

(defun custom-lsp--path->uri (path)
  (lsp--path-to-uri-1
   (-if-let ((local . remote) (-first (-lambda ((local-path . _))
                                        (s-contains? (concat (lsp-workspace-root) local-path) path))
                                      custom-lsp-path-mappings))
       (s-replace (concat (lsp-workspace-root) local) remote path)
     path)))

(defun register-custom-lsp-client (client-id)
  (if-let ((client (copy-lsp--client (gethash client-id lsp-clients))))
      (progn
        (setf (lsp--client-server-id client) (intern (concat "my-" (symbol-name client-id)))
              (lsp--client-priority client) 200
              (lsp--client-uri->path-fn client) #'custom-lsp--uri->path
              (lsp--client-path->uri-fn client) #'custom-lsp--path->uri)
        (lsp-register-client client))
    (user-error "No such client %s" client-id)))

(use-package lsp-pylsp
  :config
  (register-custom-lsp-client 'pylsp))
