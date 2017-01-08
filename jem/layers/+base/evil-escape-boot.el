;; evil-escape-boot.el

(defun jem-base-evil-escape|init ()
  (use-package evil-escape
    :ensure t
    :init
    (evil-escape-mode)
    :config
    (jem-hide-lighter evil-escape-mode)))
