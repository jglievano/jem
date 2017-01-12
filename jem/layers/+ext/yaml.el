;; +ext/yaml.el

(defun jem-ext-yaml|init ()
  (use-package yaml-mode
    :mode (("\\.yaml\\'" . yaml-mode)
           ("\\.yml\\'" . yaml-mode)
           ("\\Procfile\\'" . yaml-mode))))
