;; +ido/ido-vertical-mode.el

(defun jem-ido-ido-vertical-mode|init ()
  (use-package ido-vertical-mode
    :ensure t
    :config
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)))
