;; saveplace-boot.el

(defun jem-essentials-saveplace|init ()
  (use-package saveplace
    :ensure t
    :init
    (if (fboundp 'save-place-mode)
        (save-place-mode)
      (setq save-place t))
    (setq save-place-file (concat jem-cache-directory "places"))))
