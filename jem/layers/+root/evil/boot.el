;; evil/init.el

(defun jem-root-evil|init ()
  (use-package evil
    :ensure t
    :config
    (require 'evil)
    (evil-mode 1)))
