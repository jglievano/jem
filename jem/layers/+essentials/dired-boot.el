;; dired-boot.el

(defun jem-essentials-dired|init ()
  (jem-set-leader-keys
    "ad" 'dired
    "fj" 'dired-jump
    "jd" 'dired-jump
    "jD" 'dired-jump-other-window))
