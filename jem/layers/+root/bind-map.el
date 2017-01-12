(defun jem-root-bind-map|init ()
  (use-package bind-map
    :ensure t
    :config
    (bind-map jem-default-map
      :prefix-cmd jem-cmds
      :keys (jem-emacs-leader-key)
      :evil-keys (jem-leader-key)
      :override-minor-modes t
      :override-mode-name jem-leader-override-mode)))
