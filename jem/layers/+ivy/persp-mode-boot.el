;; persp-mode-boot.el

(defun jem-ivy-persp-mode|init ()
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
          ("X" jem-ivy-jem-layout-kill-other :exit t))))
