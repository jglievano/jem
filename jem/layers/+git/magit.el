;; +git/magit.el

(defun jem-git-magit|init ()
  (use-package magit
    :ensure t
    :config
    (jem-set-leader-keys
      "gs" 'magit-status
      "gS" 'magit-status-with-prefix)))
