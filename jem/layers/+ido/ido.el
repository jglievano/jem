;; +ido/ido.el

(defun jem-ido-ido|init ()
  (use-package ido
    :ensure t
    :demand t
    :config
    (ido-mode 1)
    (ido-everywhere 1)))
