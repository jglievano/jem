(defun jem-root-which-key|init ()
  (use-package which-key
    :ensure t
    :config
    (jem-set-leader-keys "hk" 'which-key-show-top-level)
    (let ((new-descriptions
           '(("jem-\\(.+\\)" . "\\1")
             ("jem-toggle-\\(.+\\)" . "\\1")
             ("select-window-\\([0-9]\\)" . "window \\1")
             ("jem-alternate-buffer" . "last buffer")
             ("jem-toggle-mode-line-\\(.+\\)" . "\\1")
             ("avy-goto-word-or-subword-1" . "avy word")
             ("shell-command" . "shell cmd")
             ("jem-default-pop-shell" . "open shell")
             ("jem-search-project-auto-region-or-symbol" . "search project w/input")
             ("jem-search-project-auto" . "search project")
             ("sp-split-sexp" . "split sexp")
             ("avy-goto-line" . "avy line")
             ("universal-argument" . "universal arg")
             ("er/expand-region" . "expand region")
             ("evil-lisp-state-\\(.+\\)" . "\\1")
             ("\\(.+\\)-transient-state/\\(.+\\)" . "\\2")
             ("\\(.+\\)-transient-state/body" . "\\1-transient-state"))))
      (dolist (nd new-descriptions)
        (push (cons (concat "\\`" (car nd) "\\'") (cdr nd))
              which-key-description-replacement-alist)))
    (dolist (leader-key `(,jem-leader-key ,jem-emacs-leader-key))
      (which-key-add-key-based-replacements
        (concat leader-key " m") "major mode commands"
        (concat leader-key " " jem-leader-key) "M-x"))

    (which-key-declare-prefixes
      jem-leader-key '("root" . "jem root")
      jem-emacs-leader-key '("root" . "jem root")
      (concat jem-leader-key " m")
      '("major-mode-cmd" . "Major mode commands")
      (concat jem-emacs-leader-key " m")
      '("major-mode-cmd" . "Major mode commands"))

    (which-key-setup-side-window-bottom)

    (setq which-key-special-keys nil
          which-key-use-C-h-for-paging t
          which-key-prevent-C-h-from-cycling t
          which-key-echo-keystrokes 0.02
          which-key-max-description-length 32
          which-key-sort-order 'which-key-key-order-alpha
          which-key-idle-delay 0.4
          which-key-allow-evil-operators t)

    (which-key-mode)))
