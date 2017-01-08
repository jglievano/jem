;; hi-lock-boot.el

(defun jem-base-hi-lock|init ()
  (with-eval-after-load 'hi-lock
    (jem-hide-lighter hi-lock-mode)))
