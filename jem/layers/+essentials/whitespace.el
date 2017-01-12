;; whitespace-boot.el

(defun jem-essentials-whitespace|init ()
  (use-package whitespace
    :ensure t
    :defer t
    :init
    (setq jem-show-trailing-whitespace t)
    (defun jem--show-trailing-whitespace ()
      (when jem-show-trailing-whitespace
        (set-face-attribute 'trailing-whitespace nil
                            :background
                            (face-attribute 'font-lock-comment-face
                                            :foreground))
        (setq show-trailing-whitespace 1)))
    (add-hook 'prog-mode-hook 'jem--show-trailing-whitespace)

    (jem-add-toggle whitespace
      :mode whitespace-mode
      :documentation "Display whitespace."
      :evil-leader "tw")
    (jem-add-toggle whitespace-globally
      :mode global-whitespace-mode
      :documentation "Display whitespace globally."
      :evil-leader "t C-w")

    (defun jem--set-whitespace-style-for-diff ()
      (setq-local whitespace-style '(face
                                     tabs
                                     tab-mark
                                     spaces
                                     space-mark
                                     trailing
                                     indentation::space
                                     indentation::tab
                                     newline
                                     newline-mark)))
    (add-hook 'diff-mode-hook 'whitespace-mode)
    (add-hook 'diff-mode-hook 'jem--set-whitespace-style-for-diff)
    :config
    (set-face-attribute 'whitespace-space nil
                        :background nil
                        :foreground (face-attribute 'font-lock-warning-face
                                                    :foreground))
    (set-face-attribute 'whitespace-tab nil
                        :background nil)
    (set-face-attribute 'whitespace-indentation nil
                        :background nil)))
