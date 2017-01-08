;; eldoc/boot.el

(defun jem-base-eldoc|init ()
  (use-package eldoc
    :ensure t
    :defer t
    :config
    (progn
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
      (add-hook 'ielm-mode-hook #'eldoc-mode)
      (jem-hide-lighter eldoc-mode))))
