;; evil-escape-boot.el

(defun jem-essentials-evil-escape|init ()
  (use-package evil-escape
    :ensure t
    :init
    (evil-escape-mode)
    :config
    (jem-hide-lighter evil-escape-mode)))
