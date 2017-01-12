;; ivy-boot.el

(defun jem-ivy-ivy|init ()
  (jem-set-leader-keys
    ;; TODO: post-init (evil)
    "re" 'jem-ivy-evil-registers
    ;; TODO: post-init (imenu)
    "ji" 'counsel-imenu
    ;; TODO: post-init (projectile)
    "pv" 'projectile-vc)

  (use-package ivy
    :ensure t
    :config
    (with-eval-after-load 'recentf
      (setq ivy-use-virtual-buffers t))

    (ivy-set-actions 'counsel-recentf jem--ivy-file-actions)

    (ivy-mode 1)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)

    (evil-make-overriding-map ivy-occur-mode-map 'normal)
    (ivy-set-occur 'jem-counsel-search
                   'jem--counsel-occur)
    (jem-set-leader-keys-for-major-mode 'ivy-occur-grep-mode
                                        "w" 'ivy-wgrep-change-to-wgrep-mode)
    (ido-mode -1)
    (setq projectile-completion-system 'ivy)))
