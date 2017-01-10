;; ivy-helm-make-boot.el

(defun jem-ivy-helm-make|init ()
  (use-package helm-make
    :ensure t
    :defer t
    :init
    (setq helm-make-completion-method 'ivy)
    (jem-set-leader-keys
      "cc" 'helm-make-projectile
      "cm" 'helm-make)))
