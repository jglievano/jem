;; bookmark-boot.el

(defun jem-essentials-bookmark|init ()
  (use-package bookmark
    :ensure t
    :defer t
    :init
    (setq bookmark-default-title (concat jem-cache-directory "bookmarks")
          bookmark-save-flat 1)))
