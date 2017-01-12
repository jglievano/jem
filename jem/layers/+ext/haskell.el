;; +ext/haskell.el

(defun jem-ext-haskell|init ()
  (use-package haskell-mode
    :mode (("\\.ghs\\'" . haskell-mode)
           ("\\.hsc\\'" . haskell-mode))))
