;; imenu-boot.el

(defun jem-essentials-imenu|init ()
  (use-package imenu
    :ensure t
    :defer t
    :init
    (jem-set-leader-keys "ji" 'imenu)))
