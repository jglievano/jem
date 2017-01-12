;; company-boot.el

(defun jem-essentials-company|init ()
  (use-package company
    :ensure t
    :diminish company-mode
    :defer t
    :init
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil)))
