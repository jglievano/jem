;; savehist-boot.el

(defun jem-essentials-savehist|init ()
  (use-package savehist
    :ensure t
    :init
    (setq savehist-file (concat jem-cache-directory "savehist")
          enable-recursive-minibuffers t
          history-length 1000
          savehist-additional-variables '(mark-ring
                                          global-mark-ring
                                          search-ring
                                          regexp-search-ring
                                          extended-command-history)
          savehist-autosave-interval 60)
    (savehist-mode t)))
