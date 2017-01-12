;; +ext/markdown.el

(defun jem-ext-markdown|init ()
  (use-package markdown-mode
    :mode (("\\.markdown\\'" . markdown-mode)
           ("\\.md\\'" . markdown-mode))))
