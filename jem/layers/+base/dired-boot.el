;; dired-boot.el

(defun jem-base-dired|init ()
  (jem-set-leader-keys
    "ad" 'dired
    "fj" 'dired-jump
    "jd" 'dired-jump
    "jD" 'dired-jump-other-window))
