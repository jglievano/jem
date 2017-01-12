;; ace-window-boot.el

(defun jem-essentials-ace-window|init ()
  (use-package ace-window
    :ensure t
    :defer t
    :init
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))
