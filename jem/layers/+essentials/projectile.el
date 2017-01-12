;; projectile-boot.el

(defun jem-essentials-projectile|init ()
  (use-package projectile
    :ensure t
    :commands (projectile-ack
               projectile-ag
               projectile-compile-project
               projectile-dired
               projectile-find-dir
               projectile-find-file
               projectile-find-tag
               projectile-global-mode
               projectile-test-project
               projectile-grep
               projectile-invalidate-cache
               projectile-kill-buffers
               projectile-multi-occur
               projectile-project-p
               projectile-project-root
               projectile-recentf
               projectile-regenerate-tags
               projectile-replace
               projectile-replace-regexp
               projectile-run-async-shell-command-in-root
               projectile-run-shell-command-in-root
               projectile-switch-project
               projectile-switch-to-buffer
               projectile-vc)
    :init
    (when (and (eq system-type 'windows-nt) (executable-find "find"))
      (setq projectile-indexing-method 'alien
            projectile-generic-command "find . -type f"))
    (setq projectile-sort-order 'recentf
          projectile-cache-file (concat jem-cache-directory
                                        "projectile.cache")
          projectile-known-projects-file (concat jem-cache-directory
                                                 "projectile-bookmarks.eld"))
    (projectile-global-mode 1)
    (jem-hide-lighter projectile-mode)))
