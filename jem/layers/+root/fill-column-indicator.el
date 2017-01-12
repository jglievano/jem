;; fill-column-indicator/boot.el

(defun jem-root-fill-column-indicator|init ()
  (use-package fill-column-indicator
    :ensure t
    :defer t
    :init
    (setq fci-rule-width 1)
    (setq fci-rule-color "#D0BF8F")
    (setq fci-rule-column 80)
    (push '(fci-mode "") minor-mode-alist)
    (jem-add-toggle fill-column-indicator
      :status fci-mode
      :on (turn-on-fci-mode)
      :off (turn-off-fci-mode)
      :documentation "Display the fill column indicator."
      :evil-leader "tf")))
