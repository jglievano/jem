;; projectile-boot.el

(defun jem-base-projectile|init ()
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
    (jem-set-leader-keys
      "pb" 'projectile-switch-to-buffer
      "pd" 'projectile-find-dir
      "pf" 'projectile-find-file
      "pF" 'projectile-find-file-dwim
      "pr" 'projectile-recentf
      "pp" 'projectile-switch-project
      "pv" 'projectile-vc
      "p!" 'projectile-run-shell-command-in-root
      "p&" 'projectile-run-async-shell-command-in-root
      "p%" 'projectile-replace-regexp
      "pa" 'projectile-toggle-between-implementation-and-test
      "pc" 'projectile-compile-project
      "pD" 'projectile-dired
      "pg" 'projectile-find-tag
      "p C-g" 'projectile-regenerate-tags
      "pI" 'projectile-invalidate-cache
      "pk" 'projectile-kill-buffers
      "pR" 'projectile-replace
      "pT" 'projectile-test-project)
    (projectile-global-mode 1)
    (jem-hide-lighter projectile-mode)))
