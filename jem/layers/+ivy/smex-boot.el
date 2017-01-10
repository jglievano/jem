;; smex-boot.el

(defun jem-ivy-smex|init ()
  (use-package smex
    :ensure t
    :defer t
    :init
    (setq-default smex-history-length 32
                  smex-save-file (concat jem-cache-directory
                                         ".smex-items"))))
