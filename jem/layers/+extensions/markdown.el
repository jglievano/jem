;; +extensions/markdown.el

(defun jem-extensions-markdown|init ()
  (use-package markdown-mode
    :mode (("\\.markdown\\'" . markdown-mode)
           ("\\.md\\'" . markdown-mode))))
