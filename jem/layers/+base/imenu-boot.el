;; imenu-boot.el

(defun jem-base-imenu|init ()
  (use-package imenu
    :ensure t
    :defer t
    :init
    (jem-set-leader-keys "ji" 'imenu)))
