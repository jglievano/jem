;; conf-mode-boot.el

(defun jem-essentials-conf-mode|init ()
  (add-hook 'conf-mode-hook 'jem-run-text-mode-hooks))
