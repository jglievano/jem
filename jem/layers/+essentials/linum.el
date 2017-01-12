;; linum-boot.el

(defun jem-essentials-linum|init ()
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode)
  (setq linum-format "%5d ")
  (jem-add-toggle line-numbers
    :mode linum-mode
    :documentation "Show line numbers."
    :evil-leader "tn"))
