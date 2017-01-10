;; persp-mode-boot.el

(defvar jem-layouts-directory (expand-file-name (concat jem-cache-directory
                                                        "layouts/"))
  "Save layouts in this directory.")

(defun jem-ivy-persp-mode|init ()
  (use-package persp-mode
    :ensure t
    :diminish persp-mode
    :init
    (setq persp-auto-resume-time 1
          persp-nil-name "Default"
          persp-reset-windows-on-nil-window-conf nil
          persp-set-last-persp-for-new-frames nil
          persp-save-dir jem-layouts-directory
          persp-set-ido-hooks t)

    (defun jem--activate-persp-mode ()
      "Always activate persp-mode unless it is already active."
      (unless (bound-and-true-p persp-mode)
        (persp-mode)))
    (jem-defer-after-initialization #'jem--activate-persp-mode)
    :config
    (add-hook 'ivy-ignore-buffers #'jem--layout-not-contains-buffer-p)
    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer . nil)
                    (persp-switch . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch . nil))))

    (ivy-set-actions
     'jem-ivy-jem-layouts
     '(("c" persp-kill-without-buffers "Close layout(s)")
       ("k" persp-kill "Kill layout(s)")))
    (setq jem-layouts-transient-state-remove-bindings
          '("b" "l" "C" "X"))
    (setq jem-layouts-transient-state-add-bindings
          '(("b" jem-ivy-jem-layout-buffer)
            ("l" jem-ivy-jem-layouts :exit t)
            ("C" jem-ivy-jem-layout-close-other :exit t)
            ("X" jem-ivy-jem-layout-kill-other :exit t)))))
