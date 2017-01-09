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
