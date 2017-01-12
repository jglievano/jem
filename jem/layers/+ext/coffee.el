;; +ext/coffee.el

(defun jem-ext-coffee|init ()
  (use-package coffee-mode
    :mode (("\\.coffee\\'" . coffee-mode)
           ("\\.iced\\'" . coffee-mode))))
