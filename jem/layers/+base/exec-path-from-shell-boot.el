;; exec-path-from-shell/boot.el

(defun jem-base-exec-path-from-shell|init ()
  (use-package exec-path-from-shell
    :ensure t
    :init
    (when (or (eq system-type 'darwin)
              (eq system-type 'gnu/linux)
              (memq window-system '(x)))
      (exec-path-from-shell-initialize))))
