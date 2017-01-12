;; +extensions/ruby.el

(defun jem-extensions-ruby|init ()
  (use-package ruby-mode
    :interpreter "ruby"
    :mode (("\\.rb\\'" . ruby-mode)
           ("\\.ru\\'" . ruby-mode)
           ("\\.rake\\'" . ruby-mode)
           ("\\Gemfile\\'" . ruby-mode)
           ("\\Podfile\\'" . ruby-mode)
           ("\\Vagrantfile\\'" . ruby-mode)
           ("\\Capfile\\'" . ruby-mode)
           ("\\Thorfile\\'" . ruby-mode)
           ("\\Rakefile\\'" . ruby-mode))))
