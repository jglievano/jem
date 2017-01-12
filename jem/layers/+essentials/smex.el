;; +essentials/smex.el

(defun jem-essentials-smex|init ()
  (use-package smex
    :ensure t
    :config
    (setq smex-save-file (concat jem-cache-directory "smex-items"))
    (smex-initialize)))
