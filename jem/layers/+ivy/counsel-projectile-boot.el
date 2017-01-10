;; counsel-projectile-boot.el

(defun jem-ivy-counsel-projectile|init ()
  (use-package counsel-projectile
    :ensure t
    :defer t
    :init
    (jem-use-package-add-hook projectile
      :post-init
      (progn
        (setq projectile-switch-project-action 'counsel-projectile-find-file)
        (jem-set-leader-kes
         "p SPC" 'counsel-projectile
         "pb" 'counsel-projectile-switch-to-buffer
         "pd" 'counsel-projectile-find-dir
         "pp" 'counsel-projectile-switch-project
         "pf" 'counsel-projectile-find-file
         "pr" 'projectile-recentf)))))
