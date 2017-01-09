;; eldoc/boot.el

(defun jem-essentials-eldoc|init ()
  (use-package eldoc
    :ensure t
    :defer t
    :config
    (progn
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
      (add-hook 'ielm-mode-hook #'eldoc-mode)
      (jem-hide-lighter eldoc-mode))))
