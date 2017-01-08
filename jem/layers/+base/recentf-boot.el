;; recentf-boot.el

(defun jem-base-recentf|init ()
  (use-package recentf
    :ensure t
    :defer t
    :init
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                           (recentf-mode)
                                           (recentf-track-opened-file))))
    (setq recentf-save-file (concat jem-cache-directory "recentf")
          recentf-max-saved-items 1000
          recentf-auto-cleanup 'never
          recentf-auto-save-timer (run-with-idle-timer 600 t
                                                       'recentf-save-list))
    :config
    (add-to-list 'recentf-exclude
                 (expand-file-name jem-cache-directory))
    (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")))
