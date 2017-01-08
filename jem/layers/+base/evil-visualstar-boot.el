;; evil-visualstar-boot.el

(defun jem-base-evil-visualstar|init ()
  (use-package evil-visualstar
    :ensure t
    :commands (evil-visualstar/begin-search-forward
               evil-visualstar/begin-search-backward)
    :init
    (define-key evil-visual-state-map (kbd "*")
      'evil-visualstar/begin-search-forward)
    (define-key evil-visual-state-map (kbd "#")
      'evil-visualstar/begin-search-backward)))
