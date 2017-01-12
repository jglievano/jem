;; +extensions/coffee.el

(defun jem-extensions-coffee|init ()
  (use-package coffee-mode
    :mode (("\\.coffee\\'" . coffee-mode)
           ("\\.iced\\" . coffee-mode))))
