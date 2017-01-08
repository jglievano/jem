;; init-help-fns+-boot.el

(defun jem-base-help-fns+|init ()
  (use-package help-fns+
    :ensure t
    :commands (describe-keymap)
    :init
    (jem-set-leader-keys "hdK" 'describe-keymap)))
