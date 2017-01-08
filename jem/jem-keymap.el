;; jem-keymap.el

(require 'cl-lib)

(defvar jem-default-map (make-sparse-keymap)
  "Base keymap for all jem leader key commands.")

(defun jem-declare-prefix (prefix name &optional long-name)
  "Declare a PREFIX describing a key sequence. NAME is a string used as the
prefix command. LONG-NAME if given is stored in `jem-prefix-titles'."
  (let* ((command name)
         (full-prefix (concat jem-leader-key " " prefix))
         (full-prefix-emacs (concat jem-emacs-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
         (full-prefix-emacs-lst (listify-key-sequence
                                 (kbd full-prefix-emacs))))
    (unless long-name (setq long-name name))
    (which-key-declare-prefixes
      full-prefix-emacs (cons name long-name)
      full-prefix (cons name long-name))))

(defun jem-declare-prefix-for-mode (mode prefix name &optional long-name)
  "Declares a PREFIX. MODE is where the prefix command should be added. NAME is
a symbol name used as the prefix command."
  (let ((command (intern (concat (symbol-name mode) name)))
        (full-prefix (concat jem-leader-key " " prefix))
        (full-prefix-emacs (concat jem-emacs-leader-key " " prefix))
        (is-major-mode-prefix (string-prefix-p "m" prefix))
        (major-mode-prefix (concat ", " (substring prefix 1)))
        (major-mode-prefix-emacs (concat "C-M-m " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (which-key-declare-prefixes-for-mode mode
        full-prefix-emacs prefix-name
        full-prefix prefix-name)
      (when (and is-major-mode-prefix ",")
        (which-key-declare-prefixes-for-mode mode major-mode-prefix prefix-name))
      (when (and is-major-mode-prefix "C-M-m")
        (which-key-declare-prefixes-for-mode
          mode major-mode-prefix-emacs prefix-name)))))

(defun jem-set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under `jem-leader-key'.

KEY should be a string suitable for passing to `kbd' and it should not include
the leaders. DEF is most likely a quoted command. See `define-key' for more
information about the possible choices for DEF. This function simply uses
`define-key' to add the bindings.

For convenience, this function will accept additional KEY DEF pairs. e.g:

`(jem-set-leader-keys
  \"a\" 'command1
  \"C-c\" 'command2
  \"bb\" 'command3\)"
  (while key
    (define-key jem-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(put 'jem-set-leader-keys 'lisp-indent-function 'defun)

(defun jem--create-keybinding-form (props func)
  "Returns a form to bind FUNC to a key according to PROPS.

Supported properties:

`:evil-leader'
  One or several sequence strings to be set with `jem-set-leader-keys'.

`:evil-leader-for-mode'
  One or several cons cells (MODE . KEY) where MODE is a major-mode symbol and
KEY is a key sequence string to be set with `jem-set-leader-keys-for-major-mode'.

`:global-key'
  One or several key sequence strings to be set with `global-set-key'.

`:define-key'
  One or several cons cells (MAP . KEY) where MAP is a mode map and KEY is a key
sequence string to be set with `define-key'."
  (let ((evil-leader (jem-mplist-get props :evil-leader))
        (evil-leader-for-mode (jem-mplist-get props :evil-leader-for-mode))
        (global-key (jem-mplist-get props :global-key))
        (def-key (jem-mplist-get props :define-key)))
    (append
     (when evil-leader
       `((dolist (key ',evil-leader)
           (jem-set-leader-keys key ',func))))
     (when evil-leader-for-mode
       `((dolist (val ',evil-leader-for-mode)
           (jem-set-leader-keys-for-major-mode
            (car val) (cdr val) ',func))))
     (when global-key
       `((dolist (key ',global-key)
           (global-set-key (kbd key) ',func))))
     (when def-key
       `((dolist (val ',def-key)
           (define-key (eval (car val)) (kbd (cdr val)) ',func)))))))

(provide 'jem-keymap)
