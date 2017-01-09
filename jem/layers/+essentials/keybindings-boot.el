;; keybindings-boot.el

(defun jem-essentials-keybindings|init ())

(setq jem-keybinding-prefixes '(("a" "applications")
                                ("b" "buffers")))

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
