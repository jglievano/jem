;; fill-column-indicator/boot.el

(defun jem-root-fill-column-indicator|init ()
  (use-package fill-column-indicator
    :ensure t
    :defer t
    :init
    (define-globalized-minor-mode jem-global-fci-mode
      fci-mode turn-on-fci-mode)
    (setq fci-rule-width 2)
    (setq fci-rule-character ?▒)
    (setq fci-rule-color "#212121")
    (setq fci-rule-column 80)
    (add-hook 'prog-mode-hook 'turn-on-fci-mode)
    (jem-add-toggle fill-column-indicator
      :status fci-mode
      :on (turn-on-fci-mode)
      :off (turn-off-fci-mode)
      :documentation "Display the fill column indicator."
      :evil-leader "tf")
    (jem-global-fci-mode 1)))
