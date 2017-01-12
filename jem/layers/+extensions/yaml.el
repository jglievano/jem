;; +extensions/yaml.el

(defun jem-extensions-yaml|init ()
  (use-package yaml-mode
    :mode (("\\.yaml\\'" . yaml-mode)
           ("\\.yml\\'" . yaml-mode)
           ("\\Procfile\\'" . yaml-mode))))
