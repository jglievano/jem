;; jem-transient-states.el

(defface jem-transient-state-title-face `((t :inherit mode-line))
  "Face for title of transient states.")

(defmacro jem-define-transient-state (name &rest props)
  "Define a transient state called NAME.

Available PROPS:
`:on-enter'
  Evaluate sexp argument when the transient state is switched on.
`:on-exit'
  Evaluate sexp argument when leaving the transient state.
`:doc'
  Docstring supported by `defhydra'.
`:additional-docs'
  (VARIABLE . STRING): additional docstrings to format and store in the
corresponding VARIABLE. This can be used to dynamically change the docstring.
`:title'
  Title in header of transient state.
`:columns'
  Automatically generate :doc with this number of columns.
`:hint'
  Whether to display hints or not. Defaults to nil.
`:hint-is-doc'
  Whether the hints act as doc. If non-nil the hints are displayed on the same
line as the `:title'. Default to nil.
`:dynamic-hint'
  A sexp evaluating to a string for dynamic hinting. Default to nil.
`:foreign-keys'
  What to do when keys not bound in the transient state are entered.
`:bindings'
  (STRING1 SYMBOL1 DOCSTRING :exit SYMBOL)."
  (declare (indent 1))
  (let* ((func (jem--transient-state-func-name name))
         (props-var (jem--transient-state-props-var-name name))
         (body-func (jem--transient-state-body-func-name name))
         (add-bindings
          (intern (format "jem-%s-transient-state-add-bindings" name)))
         (remove-bindings
          (intern (format "jem-%s-transient-state-remove-bindings" name)))
         (bindings (jem-mplist-get props :bindings))
         (doc (or (plist-get props :doc) "\n"))
         (title (plist-get props :title))
         (hint-var (intern (format "%s-hint" func)))
         (columns (plist-get props :columns))
         (entry-sexp (plist-get props :on-enter))
         (exit-sexp (plist-get props :on-exit))
         (hint (plist-get props :hint))
         (hint-doc-p (plist-get props :hint-is-doc))
         (dyn-hint (plist-get props :dynamic-hint))
         (additional-docs (jem-mplist-get props :additional-docs))
         (foreign-keys (plist-get props :foreign-keys))
         (bindkeys (jem--create-keybinding-form props body-func)))
    `(progn
       (defvar ,props-var nil
         ,(format (concat "Association list containing a copy of some "
                          "properties of the transient state %S. Those "
                          "properties are used in macro "
                          "`jem-transient-state-format-hint'.") name))
       (add-to-list ',props-var '(hint ,hint))
       (add-to-list ',props-var '(columns ,columns))
       (add-to-list ',props-var '(foreign-keys ,foreign-keys))
       (add-to-list ',props-var '(entry-sexp ,entry-sexp))
       (add-to-list ',props-var '(exit-sexp ,exit-sexp))
       (jem-defer-after-initialization
        '(lambda ()
           (eval
            (append
             '(defhydra ,func
                (nil nil
                     :hint ,hint
                     :columns ,columns
                     :foreign-keys ,foreign-keys
                     :body-pre ,entry-sexp
                     :before-exit ,exit-sexp)
                ,doc)
             (jem--transient-state-adjust-bindings
              ',bindings ',remove-bindings ',add-bindings)))
           (when ,title
             (let ((guide (concat "[" (propertize "KEY" 'face 'hydra-face-blue)
                                  "] exits state  ["
                                  (if ',foreign-keys
                                      (propertize "KEY" 'face 'hydra-face-pink)
                                    (propertize "KEY" 'face 'hydra-face-red))
                                  "] will not exit")))
               (add-face-text-property 0 (length guide) 'italic t guide)
               (setq ,hint-var
                     (list 'concat
                           (concat
                            (propertize ,title
                                        'face
                                        'jem-transient-state-title-face)
                            (if ,hint-doc-p " " "\n"));; ,hint-var
                           ',dyn-hint
                           (concat "\n" guide)))))
           ,@bindkeys)))))

(defun jem--transient-state-adjust-bindings (bindings to-remove to-add)
  (append
   (cl-remove-if
    (lambda (bnd)
      (and (boundp to-remove)
           (listp (symbol-value to-remove))
           (member (car bnd) (symbol-value to-remove))))
    bindings)
   (when (and (boundp to-add)
              (listp (symbol-value to-add)))
     (symbol-value to-add))))

(defun jem--transient-state-body-func-name (name)
  "Return the name of the transient state function."
  (intern (format "jem-%S-transient-state/body" name)))

(defmacro jem--transient-state-format-hint (name var hint)
  "Format HINT and store the result in VAR for transient state NAME."
  (declare (indent 1))
  (let* ((props-var ,(jem--transient-state-props-var-name name))
         (prop-hint (cadr (assq 'hint props-var)))
         (prop-columns (cadr (assq 'columns props-var)))
         (prop-foreign-keys (cadr (assq 'foreign-keys props-var)))
         (prop-entry-sexp (cadr (assq 'entry-sexp props-var)))
         (prop-exit-sexp (cadr (assq 'entry-sexp props-var))))
    (setq ,var (jem--transient-state-make-doc
                ',name
                ,hint
                `(nil
                  nil
                  :hint ,prop-hint
                  :columns ,prop-columns
                  :foreign-keys ,prop-foreign-keys
                  :body-pre ,prop-entry-sexp
                  :before-exit ,prop-exit-sexp)))
    'append))

(defun jem--transient-state-func-name (name)
  "Return a full transient state function name given a NAME."
  (intern (format "jem-%S-transient-state" name)))

(defun jem--transient-state-heads-name (name)
  "Return the name of the transient state heads variable."
  (intern (format "jem-%S-transient-state/heads" name)))

(defun jem--transient-state-make-doc (transient-state docstring &optional body)
  "Use `hydra' internal function to format and apply DOCSTRING."
  (let ((heads (jem--transient-state-heads-name transient-state)))
    (setq body (if body body '(nil nil :hint nil :foreign-keys nil)))
    (eval
     (hydra--format nil body docstring (symbol-value heads)))))

(defun jem--transient-state-props-var-name (name)
  "Return the name of the variable used to store the transient state properties."
  (intern (format "jem--%S-transient-state-props" name)))

(provide 'jem-transient-states)
