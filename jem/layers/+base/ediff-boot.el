;; ediff/boot.el

(defun jem-base-ediff|init ()
  (use-package ediff
    :ensure t
    :defer t
    :init
    (setq-default
     ediff-window-setup-function 'ediff-setup-window-plain
     ediff-split-window-function 'split-window-horizontally)
    (require 'outline)
    (add-hook 'ediff-prepare-buffer-hook #'show-all)
    (add-hook 'ediff-quit-hook #'winner-undo)))
