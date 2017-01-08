;; bookmark-boot.el

(defun jem-base-bookmark|init ()
  (use-package bookmark
    :ensure t
    :defer t
    :init
    (setq bookmark-default-title (concat jem-cache-directory "bookmarks")
          bookmark-save-flat 1)
    (jem-set-leader-keys "fb" 'bookmark-jump)))
