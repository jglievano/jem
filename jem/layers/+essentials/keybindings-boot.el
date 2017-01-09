;; keybindings-boot.el

(defun jem-essentials-keybindings|init ())

(setq jem-keybinding-prefixes '(("a" "applications")
                                ("b" "buffers")
                                ("c" "compile/comments")
                                ("e" "errors")
                                ("f" "files")
                                ("fC" "files/convert")
                                ("fv" "variables")
                                ("h" "help")
                                ("hd" "help-describe")
                                ("i" "insertion")
                                ("j" "jump/join/split")
                                ("n" "narrow/numbers")
                                ("p" "projects")
                                ("p$" "projects/shell")
                                ("q" "quit")
                                ("t" "toggles")
                                ("T" "UI toggles")
                                ("w" "windows")
                                ("x" "text")
                                ("xa" "align")
                                ("xd" "delete")
                                ("xl" "lines")
                                ("xm" "move")
                                ("xt" "transpose")
                                ("xw" "words")))

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

;; universal argument.
(jem-set-leader-keys "u" 'universal-argument)
(define-key universal-argument-map
  (kbd (concat jem-leader-key " u")) 'universal-argument-more)

;; shell.
(jem-set-leader-keys "!" 'shell-command)

;; applications.
(jem-set-leader-keys
  "ac" 'calc-dispatch
  "ap" 'list-processes
  "aP" 'proced
  "au" 'undo-tree-visualize)

;; buffers.
(jem-set-leader-keys
  "TAB" 'jem-alternate-buffers
  "bd" 'jem-kill-this-buffer
  "be" 'jem-safe-erase-buffer
  "bh" 'jem-home
  "b C-d" 'jem-kill-matching-buffers-rudely
  "bn" 'next-buffer
  "bm" 'jem-kill-other-buffers
  "bN" 'jem-new-empty-buffer
  "bP" 'jem-copy-clipboard-to-whole-buffer
  "bp" 'previous-buffer
  "bR" 'jem-safe-revert-buffer
  "bs" 'jem-switch-to-scratch-buffer
  "bY" 'jem-copy-whole-buffer-to-clipboard
  "bw" 'read-only-mode)

;; errors.
(jem-set-leader-keys
  "en" 'jem-next-error
  "eN" 'jem-previous-error
  "ep" 'jem-previous-error)

(jem-define-transient-state error
  :title "Error transient state"
  :hint-is-doc t
  :dynamic-hint
  (let ((sys (jem-error-delegate)))
    (cond
     ((eq 'flycheck sys)
      "\nBrowsing flycheck errors from this buffer.")
     ((eq 'emacs sys)
      (let ((buf (next-error-find-buffer)))
        (if buf
            (concat "\nBrowsing entries from \""
                    (buffer-name buf)
                    "\""
                    (with-current-buffer buf
                      (when jem--gne-line-func
                        (format " (%d of %d)"
                                (max 1 (1+ (- jem--gne-cur-line
                                              jem--gne-min-line)))
                                (1+ (- jem--gne-max-line
                                       jem--gne-min-line))))))
          "\nNo next-error capable buffer found.")))))
  :bindings
  ("n" jem-next-error "next")
  ("p" jem-previous-error "prev")
  ("q" nil "quit" :exit t)
  :evil-leader "e.")

;; file.
(jem-set-leader-keys
  "fc" 'jem-copy-file
  "fD" 'jem-delete-current-buffer-file
  "fei" 'jem-find-user-init-file
  "fed" 'jem-find-dotfile
  "feD" 'jem-ediff-dotfile-and-template
  "fev" 'jem-display-and-copy-version
  "fCd" 'jem-unix2dos
  "fCu" 'jem-dos2unix
  "fg" 'rgrep
  "fl" 'find-file-literally
  "fE" 'jem-sudo-edit
  "fo" 'jem-open-file-or-directory-in-external-app
  "fR" 'jem-rename-current-buffer-file
  "fS" 'evil-write-all
  "fs" 'save-buffer
  "fvd" 'add-dir-local-variable
  "fvf" 'add-file-local-variable
  "fvp" 'add-file-local-variable-prop-line
  "fy" 'jem-show-and-copy-buffer-filename)

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

;; insert.
(jem-set-leader-keys
  "iJ" 'jem-insert-line-below-no-indent
  "iK" 'jem-insert-line-above-no-indent
  "ik" 'jem-evil-insert-line-above
  "ij" 'jem-evil-insert-line-below)

;; format
(jem-set-leader-keys
  "jo" 'open-line
  "j=" 'jem-indent-region-or-buffer
  "jS" 'jem-split-and-new-line
  "jk" 'jem-evil-goto-next-line-and-indent)

;; navigation.
(jem-set-leader-keys
  "j0" 'jem-push-mark-and-goto-beginning-of-line
  "j$" 'jem-push-mark-and-goto-end-of-line
  "jf" 'find-function
  "jv" 'find-variable)

;; compilation.
(jem-set-leader-keys
  "cC" 'compile
  "ck" 'kill-compilation
  "cr" 'recompile
  "cd" 'jem-close-compilation-window)

(with-eval-after-load 'compile
  (define-key compilation-mode-map "r" 'recompile)
  (define-key compilation-mode-map "g" nil))

;; narrow and widen.
(jem-set-leader-keys
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen)

;; toggle.
(jem-add-toggle highlight-current-line-globally
  :mode global-hl-line-mode
  :documentation "Globally highlight the current line."
  :evil-leader "thh")
(jem-add-toggle truncate-lines
  :status truncate-lines
  :on (toggle-truncate-lines)
  :off (toggle-truncate-lines -1)
  :documentation "Truncate long lines (no wrap)."
  :evil-leader "tl")
(jem-add-toggle visual-line-navigation
  :status visual-line-mode
  :on
  (progn
    (visual-line-mode)
    (evil-define-minor-mode-key 'motion 'visual-line-mode "j"
      'evil-next-visual-line)
    (evil-define-minor-mode-key 'motion 'visual-line-mode "k"
      'evil-previous-visual-line)
    (when (bound-and-true-p evil-escape-mode)
      (evil-escape-mode -1)
      (setq evil-escape-motion-state-shadowed-func nil)
      (evil-define-minor-mode-key 'motion 'visual-line-mode "j"
        'evil-next-visual-line)
      (evil-define-minor-mode-key 'motion 'visual-line-mode "k"
        'evil-previous-visual-line)
      (evil-escape-mode))
    (evil-normalize-keymaps))
  :off
  (progn
    (visual-line-mode -1)
    (evil-normalize-keymaps))
  :documentation "Move point according to visual lines."
  :evil-leader "tL")
(jem-add-toggle auto-fill-mode
  :status auto-fill-function
  :on (auto-fill-mode)
  :off (auto-fill-mode -1)
  :documentation "Break line beyong `current-fill-column' while editing."
  :evil-leader "tF")
(jem-add-toggle debug-on-error
  :status debug-on-error
  :on (setq debug-on-error t)
  :off (setq debug-on-error nil)
  :documentation "Toggle display of backtrace when an error happens."
  :evil-leader "Tf")
(jem-add-toggle fringe
  :status (not (equal fringe-mode 0))
  :on (call-interactively 'fringe-mode)
  :off (fringe-mode 0)
  :documentation "Display the fringe in GUI mode."
  :evil-leader "Tf")
(jem-add-toggle fullscreen-frame
  :status (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
  :on (jem-toggle-frame-fullscreen)
  :off (jem-toggle-frame-fullscreen)
  :documentation "Display the current frame in full screen."
  :evil-leader "TF")
(jem-add-toggle maximize-frame
  :status (eq (frame-parameter nil 'fullscreen) 'maximized)
  :on (toggle-frame-maximized)
  :off (toggle-frame-maximized)
  :documentation "Maximize the current frame."
  :evil-leader "TM")
(jem-add-toggle semantic-stickyfunc
  :mode semantic-stickyfunc-mode
  :documentation "Enable semantic-stickyfunc."
  :evil-leader "TS")
(jem-add-toggle semantic-stickyfunc-globally
  :mode global-semantic-stickyfunc-mode
  :documentation "Enable semantic-stickyfunc globally."
  :evil-leader "T C-S")

;; quit.
(jem-set-leader-keys
  "qs" 'jem-save-buffers-kill-emacs
  "qq" 'jem-prompt-kill-emacs
  "qQ" 'jem-kill-emacs
  "qz" 'jem-frame-killer)

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
  "w2" 'jem-layout-double-columns
  "w3" 'jem-layout-triple-columns
  "wb" 'jem-switch-to-minibuffer-window
  "wd" 'jem-delete-window
  "wt" 'jem-toggle-current-window-dedication
  "wf" 'follow-mode
  "wF" 'make-frame
  "wH" 'evil-window-move-far-left
  "w <S-left>" 'evil-window-move-far-left
  "wh" 'evil-window-left
  "w <left>" 'evil-window-left
  "wJ" 'evil-window-move-very-bottom
  "w <S-down>" 'evil-window-move-very-bottom
  "wj" 'evil-window-down
  "w <down>" 'evil-window-down
  "wK" 'evil-window-move-very-top
  "w <S-up>" 'evil-window-move-very-top
  "wk" 'evil-window-up
  "w <up>" 'evil-window-up
  "wL" 'evil-window-move-far-right
  "w <S-right>" 'evil-window-move-far-right
  "wl" 'evil-window-right
  "w <right>" 'evil-window-right
  "wm" 'jem-toggle-maximize-buffer
  "wc" 'jem-toggle-centered-buffer-mode
  "wC" 'jem-centered-buffer-mode-full-width
  "wo" 'other-frame
  "wr" 'jem-rotate-windows
  "wR" 'jem-rotate-windows-backward
  "ws" 'split-window-below
  "wS" 'split-window-below-and-focus
  "w-" 'split-window-below
  "wU" 'winner-redo
  "wu" 'winner-undo
  "wv" 'split-window-right
  "wV" 'split-window-right-and-focus
  "ww" 'other-window
  "w/" 'split-window-right
  "w=" 'balance-window
  "w_" 'jem-maximize-horizontally)

;; text.
(defalias 'count-region 'count-words-region)
(jem-set-leader-keys
  "xa&" 'jem-align-repeat-ampersand
  "xa(" 'jem-align-repeat-left-paren
  "xa)" 'jem-align-repeat-right-paren
  "xa," 'jem-align-repeat-comma
  "xa." 'jem-align-repeat-decimal
  "xa:" 'jem-align-repeat-colon
  "xa;" 'jem-align-repeat-semicolon
  "xa=" 'jem-align-repeat-equal
  "xa\\" 'jem-align-repeat-backslash
  "xaa" 'align
  "xac" 'align-current
  "xam" 'jem-align-repeat-math-oper
  "xar" 'jem-align-repeat
  "xa|" 'jem-align-repeat-bar
  "xc" 'count-region
  "xdw" 'delete-trailing-whitespace
  "xjc" 'set-justification-center
  "xjf" 'set-justification-full
  "xjl" 'set-justification-left
  "xjn" 'set-justification-none
  "xjr" 'set-justification-right
  "xls" 'jem-sort-lines
  "xlu" 'jem-uniquify-lines
  "xtc" 'transpose-chars
  "xtl" 'transpose-lines
  "xtw" 'transpose-words
  "xU" 'upcase-region
  "xu" 'downcase-region
  "xwc" 'jem-count-words-analysis
  "x TAB" 'indent-rigidly)
(define-key indent-rigidly-map "h" 'indent-rigidly-left)
(define-key indent-rigidly-map "l" 'indent-rigidly-right)
(define-key indent-rigidly-map "H" 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map "L" 'indent-rigidly-right-to-tab-stop)

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
  ("K" jem-kill-this-buffer "kill")
  ("q" nil "quit" :exit t))
(jem-set-leader-keys "b." 'jem-buffer-transient-state-body)

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
  485  ──────^^^^───────────── ────^^^^───────────── ─────^^─────────────── ──────^^──────────────────── ─────^^──────────────────────────────
  486  [_j_/_k_] down/up       [_J_/_K_] down/up     [_s_] vertical         [_[_] shrink horizontally    [_q_] quit
  487  [_h_/_l_] left/right    [_H_/_L_] left/right  [_S_] vert & follow    [_]_] enlarge horizontally   [_u_] restore prev layout
  488  [_0_-_9_] window N      [_r_]^^   rotate fwd  [_v_] horizontal       [_{_] shrink vertically      [_U_] restore next layout
  489  [_w_]^^   other window  [_R_]^^   rotate bwd  [_V_] horiz & follow   [_}_] enlarge vertically     [_d_] close current
  490  [_o_]^^   other frame   ^^^^                  ^^                     ^^                           [_D_] close other"
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
  ("r" jem-rotate-windows)
  ("R" jem-rotate-windows-backward)
  ("s" split-window-below)
  ("S" split-window-below-and-focus)
  ("u" winner-undo)
  ("U" winner-redo)
  ("v" split-window-right)
  ("V" split-window-right-and-focus)
  ("w" other-window))
(jem-set-leader-keys "w."
  'jem-window-manipulation-transient-state-body)
