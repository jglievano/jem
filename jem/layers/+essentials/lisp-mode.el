;; lisp-mode-boot.el

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defun jem-essentials-lisp-mode|init ()
  (use-package lisp-mode
    :defer t
    :preface
    (defvar lisp-modes '(emacs-lisp-mode
                         inferior-emacs-lisp-mode
                         ielm-mode
                         lisp-mode
                         inferior-lisp-mode
                         lisp-interaction-mode
                         slime-repl-mode))

    (defvar lisp-mode-hooks
      (mapcar (function (lambda (mode)
                          (intern
                           (concat (symbol-name mode) "-hook"))))
              lisp-modes))
    (defvar lisp-mode-initialized nil)
    (declare-function paredit-mode "ext:paredit")
    (defface esk-paren-face
      '((((class color) (background dark))
         (:foreground "grey50"))
        (((class color) (background light))
         (:foreground "grey55")))
      "Face used to dim parentheses."
      :group 'starter-kit-faces)
    (defun jem-lisp-mode-hook ()
      (unless lisp-mode-initialized
        (setq lisp-mode-initialized t))
      (company-mode 1)
      (paredit-mode 1))
    :init
    (show-paren-mode 1)
    (mapc
     (lambda (major-mode)
       (font-lock-add-keywords
        major-mode
        '(("(\\(lambda\\)\\>"
           (0 (ignore
               (compose-region (match-beginning 1)
                               (match-end 1)
                               ?λ))))
          ("(\\|)" . 'esk-paren-face)
          ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
           (1 font-lock-keyword-face)
           (2 font-lock-function-name-face nil t)))))
     lisp-modes)

    (apply #'hook-into-modes 'jem-lisp-mode-hook lisp-mode-hooks)))
