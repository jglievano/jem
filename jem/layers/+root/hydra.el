(defun jem-root-hydra|hydra-key-doc-function (key key-width doc doc-width)
  "Custom hint documentation format for keys."
  (format (format "[%%%ds] %%%ds" key-width (- -1 doc-width))
          key doc))

(defun jem-root-hydra|init ()
  (use-package hydra
    :ensure t
    :config
    (setq hydra-key-doc-function 'jem-root-hydra|hydra-key-doc-function
          hydra-head-format "[%s] ")))
