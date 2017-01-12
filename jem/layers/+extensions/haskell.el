;; haskell.el

(defun jem-extensions-haskell|init ()
  (use-package haskell-mode
    :mode (("\\.ghs\\'" . haskell-mode)
           ("\\.hsc\\'" . haskell-mode))))
