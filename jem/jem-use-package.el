;; jem-use-package.el

(defconst jem--use-package-add-hook-keywords '(:pre-init
                                               :post-init
                                               :pre-config
                                               :post-config))

(defmacro jem-use-package-add-hook (name &rest plist)
  "Add post hooks to `:init' or `:config' arguments of an existing configuration.

In order to use this macro, `use-package-inject-hooks' must be non-nil.

Usage:

  (jem-use-package-add-hook package-name
    [:keyword [option]]...)

:pre-init    Code to run before the default `:init'.
:post-init   Code to run after the default `:init'.
:pre-config  Code to run before the default `:config'.
:post-config Code to run after the default `:config'."
  (declare (indent 1))
  (let ((name-symbol (if (stringp name) (intern name) name))
        (expanded-forms '()))
    (dolist (keyword jem--use-package-add-hook-keywords)
      (let ((body (jem-mplist-get plist keyword)))
        (when body
          (let ((hook (intern (format "use-package--%S--%s-hook"
                                      name-symbol
                                      (substring (format "%s" keyword) 1)))))
            (push `(add-hook ',hook (lambda nil ,@body)) expanded-forms)))))
    `(progn ,@expanded-forms)))

(provide 'jem-use-package)
