;; +git/magit.el

(defun jem-git-magit|init ()
  (use-package magit
    :ensure t
    :config
    (jem-set-leader-keys
      "gl" 'magit-log-buffer-file
      "gs" 'magit-status)))
