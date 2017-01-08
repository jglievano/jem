;; conf-mode-boot.el

(defun jem-base-conf-mode|init ()
  (add-hook 'conf-mode-hook 'jem-run-text-mode-hooks))
