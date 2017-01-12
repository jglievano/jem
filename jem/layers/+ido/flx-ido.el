;; +ido/flx-ido.el

(defun jem-ido-flx-ido|init ()
  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode 1)
    (setq ido-enable-flex-matching t)))
