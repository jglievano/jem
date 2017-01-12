;; +ext/python.el

(defun jem-ext-python|init ()
  (use-package python-mode
    :functions python-shell
    :mode (("\\.py\\'" . python-mode)
           ("\\BUILD\\'" . python-mode)
           ("\\.gyp\\'" . python-mode)
           ("\\.gypi\\'" . python-mode))
    :interpreter ("python" . python-mode)))
