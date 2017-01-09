;; page-break-lines-boot.el

(defun jem-essentials-page-break-lines|init ()
  (use-package page-break-lines
    :ensure t
    :config
    (global-page-break-lines-mode t)
    (jem-hide-lighter page-break-lines-mode)))
