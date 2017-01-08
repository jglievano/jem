;; ace-window-boot.el

(defun jem-ace-delete-window (&optional arg)
  "Ace delete window."
  (interactive "P")
  (require 'ace-window)
  (aw-select
   " Ace - Delete Window"
   (lambda (window)
     (when (equal '(4) arg)
       (with-selected-window window
         (jem-kill-this-buffer arg)))
     (aw-delete-window window))))

(defun jem-ace-kill-this-buffer (&optional arg)
  "Ace kill visible buffer in a window.

If the universal prefix argument is used then kill the window too."
  (interactive "P")
  (require 'ace-window)
  (let (golden-ratio-mode)
    (aw-select
     " Ace - Kill buffer in Window"
     (lambda (window)
       (with-selected-window window
         (jem-kill-this-buffer arg))))))

(defun jem-base-ace-window|init ()
  (use-package ace-window
    :ensure t
    :defer t
    :init
    (jem-set-leader-keys
      "bD" 'jem-ace-kill-this-buffer
      "wD" 'jem-ace-delete-window
      "wM" 'ace-swap-window
      "wW" 'ace-window)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(defun jem-kill-this-buffer (&optional arg)
  "Kill the current buffer.

If the universal prefix argument is used then kill the window too."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))
