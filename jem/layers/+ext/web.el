;; +ext/web.el

(defun jem-ext-web|init ()
  (use-package web-mode
    :mode (("\\.html\\'" . web-mode)
           ("\\.htm\\'" . web-mode)
           ("\\.hbs\\'" . web-mode)
           ("\\.ejs\\'" . web-mode)
           ("\\.php\\'" . web-mode))))
