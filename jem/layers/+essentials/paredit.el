;; paredit-boot.el

(defun jem-essentials-paredit|init ()
  (use-package paredit
    :ensure t
    :commands paredit-mode
    :diminish paredit-mode
    :config
    (use-package paredit-ext)))
