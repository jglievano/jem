;; keybindings-boot.el

(defun jem-essentials-keybindings|init ()
  (setq jem-keybinding-prefixes '(("b" "buffers")
                                  ("d" "debug")
                                  ("e" "emacs")
                                  ("f" "files")
                                  ("h" "help")
                                  ("hd" "help-describe")
                                  ("t" "text")
                                  ("w" "windows")
                                  ("p" "projects")
                                  ("w" "windows")
                                  ("!" "shell")))

  (mapc (lambda (x) (apply #'jem-declare-prefix x))
        jem-keybinding-prefixes)

  ;; instantly display current keystrokes in mini-buffer.
  (setq echo-keystrokes 0.02)
  ;; auto-indent on RET.
  (define-key global-map (kbd "RET") 'newline-and-indent)

  ;; alternative to search next.
  (define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
  ;; escape from isearch-mode.
  (define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

  ;; escape.
  (define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

  ;; shell.
  (jem-set-leader-keys "!!" 'shell-command)

  ;; buffers.
  (jem-set-leader-keys
    "b TAB" 'jem-alternate-buffers
    "bb" 'ivy-switch-buffer
    "bd" 'jem-kill-this-buffer
    "bn" 'jem-new-empty-buffer
    "bCv" 'jem-copy-clipboard-to-whole-buffer
    "bCc" 'jem-copy-whole-buffer-to-clipboard
    "br" 'read-only-mode)

  ;; emacs.
  (jem-set-leader-keys
    "ek" 'jem-switch-to-scratch-buffer
    "eq" 'jem-prompt-kill-emacs)

  ;; file.
  (jem-set-leader-keys
    "fc" 'jem-copy-file
    "fd" 'jem-delete-current-buffer-file
    "fF" 'jem-open-file-or-directory-in-external-app
    "fr" 'jem-rename-current-buffer-file
    "fs" 'save-buffer)

  ;; help.
  (jem-set-leader-keys
    "hdb" 'describe-bindings
    "hdc" 'describe-char
    "hdf" 'describe-function
    "hdk" 'describe-key
    "hdl" 'jem-describe-last-keys
    "hdp" 'describe-package
    "hds" 'jem-describe-system-info
    "hdt" 'describe-theme
    "hdv" 'describe-variable
    "hn" 'view-emacs-news)

  ;; text.
  (jem-set-leader-keys
    "tg" 'avy-goto-line
    "ti" 'jem-indent-region-or-buffer
    "ta&" 'jem-align-repeat-ampersand
    "ta(" 'jem-align-repeat-left-paren
    "ta)" 'jem-align-repeat-right-paren
    "ta," 'jem-align-repeat-comma
    "ta." 'jem-align-repeat-decimal
    "ta:" 'jem-align-repeat-colon
    "ta;" 'jem-align-repeat-semicolon
    "ta=" 'jem-align-repeat-equal
    "ta\\" 'jem-align-repeat-backslash
    "taa" 'align
    "tac" 'align-current
    "tam" 'jem-align-repeat-math-oper
    "tar" 'jem-align-repeat
    "ta|" 'jem-align-repeat-bar
    "tdw" 'delete-trailing-whitespace
    "tjc" 'set-justification-center
    "tjf" 'set-justification-full
    "tjl" 'set-justification-left
    "tjn" 'set-justification-none
    "tjr" 'set-justification-right
    "ts" 'jem-sort-lines
    "tu" 'jem-uniquify-lines)

  ;; navigation.
  (jem-set-leader-keys
    "j0" 'jem-push-mark-and-goto-beginning-of-line
    "j$" 'jem-push-mark-and-goto-end-of-line
    "jf" 'find-function
    "jv" 'find-variable)

  ;; debug..
  (jem-set-leader-keys
    "dC" 'compile
    "dk" 'kill-compilation
    "dr" 'recompile
    "dd" 'jem-close-compilation-window)

  (with-eval-after-load 'compile
    (define-key compilation-mode-map "r" 'recompile)
    (define-key compilation-mode-map "g" nil))

  ;; window.
  (defun split-window-below-and-focus ()
    "Split the window vertically and focus."
    (interactive)
    (split-window-below)
    (windmove-down)
    (when (and (boundp 'golden-ratio-mode)
               (symbol-value golden-ratio-mode))
      (goldern-ratio)))
  (defun split-window-right-and-focus ()
    "Split the window horizontally and focus."
    (interactive)
    (split-window-right)
    (windmove-right)
    (when (and (boundp 'golden-ratio-mode)
               (symbol-value golden-ratio-mode))
      (golden-ratio)))
  (jem-set-leader-keys
    "wa" 'ace-window
    "wb" 'jem-switch-to-minibuffer-window
    "wd" 'jem-delete-window
    "wf" 'follow-mode
    "wh" 'evil-window-left
    "w <left>" 'evil-window-left
    "wj" 'evil-window-down
    "w <down>" 'evil-window-down
    "wk" 'evil-window-up
    "w <up>" 'evil-window-up
    "wl" 'evil-window-right
    "w <right>" 'evil-window-right
    "wo" 'other-frame
    "wS" 'split-window-below-and-focus
    "w-" 'split-window-below
    "wV" 'split-window-right-and-focus
    "w/" 'split-window-right)

  ;; shell.
  (with-eval-after-load 'shell
    (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
    (evil-define-key 'insert comint-mode-map [down] 'comint-next-input))

  ;; buffer transient state.
  (jem-define-transient-state buffer
    :title "Buffer selection transient state"
    :bindings
    ("n" next-buffer "next")
    ("N" previous-buffer "previous")
    ("p" previous-buffer "previous")
    ("K" kill-this-buffer "kill")
    ("q" nil "quit" :exit t))
  (jem-set-leader-keys "b." 'jem-buffer-transient-state/body)

  ;; window manipulation transient state.
  (defun jem-shrink-window-horizontally (delta)
    (interactive "p")
    (shrink-window delta t))
  (defun jem-shrink-window (delta)
    (interactive "p")
    (shrink-window delta t))
  (defun jem-enlarge-window (delta)
    (interactive "p")
    (enlarge-window delta))
  (defun jem-enlarge-window-horizontally (delta)
    (interactive "p")
    (enlarge-window delta t))
  (jem-define-transient-state window-manipulation
    :title "Window manipulation transient state"
    :doc "
 Select^^^^              Move^^^^              Split^^                Resize^^                     Other^^
 ──────^^^^───────────── ────^^^^───────────── ─────^^─────────────── ──────^^──────────────────── ─────^^──────────────────────────────
 [_j_/_k_] down/up       [_J_/_K_] down/up     [_s_] vertical         [_[_] shrink horizontally    [_q_] quit
 [_h_/_l_] left/right    [_H_/_L_] left/right  [_S_] vert & follow    [_]_] enlarge horizontally   [_u_] restore prev layout
 [_0_-_9_] window N                            [_v_] horizontal       [_{_] shrink vertically      [_U_] restore next layout
 [_w_]^^   other window                        [_V_] horiz & follow   [_}_] enlarge vertically     [_d_] close current
 [_o_]^^   other frame   ^^^^                  ^^                     ^^                           [_D_] close other"
    :bindings
    ("q" nil :exit t)
    ("0" select-window-0)
    ("1" select-window-1)
    ("2" select-window-2)
    ("3" select-window-3)
    ("4" select-window-4)
    ("5" select-window-5)
    ("6" select-window-6)
    ("7" select-window-7)
    ("8" select-window-8)
    ("9" select-window-9)
    ("-" split-window-below-and-focus)
    ("/" split-window-right-and-focus)
    ("[" jem-shrink-window-horizontally)
    ("]" jem-enlarge-window-horizontally)
    ("{" jem-shrink-window)
    ("}" jem-enlarge-window)
    ("d" delete-window)
    ("D" delete-other-windows)
    ("h" evil-window-left)
    ("<left>" evil-window-left)
    ("j" evil-window-down)
    ("<down>" evil-window-down)
    ("k" evil-window-up)
    ("<up>" evil-window-up)
    ("l" evil-window-right)
    ("<right>" evil-window-right)
    ("H" evil-window-move-far-left)
    ("<S-left>" evil-window-move-far-left)
    ("J" evil-window-move-very-bottom)
    ("<S-down>" evil-window-move-very-bottom)
    ("K" evil-window-move-very-top)
    ("<S-up>" evil-window-move-very-top)
    ("L" evil-window-move-far-right)
    ("<S-right>" evil-window-move-far-right)
    ("o" other-frame)
    ("s" split-window-below)
    ("S" split-window-below-and-focus)
    ("u" winner-undo)
    ("U" winner-redo)
    ("v" split-window-right)
    ("V" split-window-right-and-focus)
    ("w" other-window))
  (jem-set-leader-keys "w."
    'jem-window-manipulation-transient-state/body))
