;; jem-toggle.el

(defvar jem-toggles '()
  "List of all declared toggles. The structure of an element is a property list
(name :func FUNCTION :doc STRING :key STRING).")

(defmacro jem-add-toggle (name &rest props)
  "Add a toggle with NAME.

This macro creates the following functions:
- jem-toggle-NAME switches on and off.
- jem-toggle-NAME-on toggles on if off.
- jem-toggle-NAME-off toggles off if on.

Available PROPS:

`:status'
  The expression to evaluate to get the current status.

`:if'
  If nil then no attempt to update the toggle status will be performed.

`:on'
  Evaluated when toggle is on.

`:off'
  Evaluated when toggle is off.

`:documenation'
  Describes what the toggle does.

`:prefix'
  Bound to the raw value of prefix-arg (same as calling (interactive \"P\")) in
the wrapper function.

`:on-message'
  Evaluated and displayed when the on toggle is activated.

`:mode'
  Must be a minor mode if given. Overrides `:on', `:off' and `:status'."
  (declare (indent 1))
  (let* ((wrapper-func (intern (format "jem-toggle-%s"
                                       (symbol-name name))))
         (wrapper-func-status (intern (format "%s-p" wrapper-func)))
         (wrapper-func-on (intern (format "%s-on" wrapper-func)))
         (wrapper-func-off (intern (format "%s-off" wrapper-func)))
         (mode (plist-get props :more))
         (status (or mode (plist-get props :status)))
         (condition (plist-get props :if))
         (doc (plist-get props :documentation))
         (on-body (if mode `((,mode)) (jem-mplist-get props :on)))
         (off-body (if mode `((,mode -1)) (jem-mplist-get props :off)))
         (prefix-arg-var (plist-get props :prefix))
         (on-message (plist-get props :on-message))
         (bindkeys (jem--create-keybinding-form props wrapper-func))
         (status-eval `(and (or (and (symbolp ',status) (boundp ',status))
                                (listp ',status))
                            ,status)))
    `(progn
       (push (append '(,name) '(:function ,wrapper-func
                                          :predicte ,wrapper-func-status)
                     ',props) jem-toggles)
       (defun ,wrapper-func ,(if prefix-arg-var (list prefix-arg-var) ())
         ,(format "Toggle %s on and off." (symbol-name name))
         ,(if prefix-arg-var '(interactive "P") '(interactive))
         (if (or (null ',condition)
                 (and (or (and (symbolp ',condition) (boundp ',condition))
                          (listp ',condition))
                      ,condition))
             (if (,wrapper-func-status)
                 (progn ,@off-body
                        (when (called-interactively-p 'any)
                          (message ,(format "%s disabled." name))))
               ,@on-body
               (when (called-interactively-p 'any)
                 (message ,(or on-message (format "%s enabled." name)))))
           (message "This toggle is not supported.")))
       (defun ,wrapper-func-status ()
         ,(format "Check if %s is on." (symbol-name name))
         ,status-eval)
       ,@(when status
           `((defun ,wrapper-func-on ()
               ,(format "Toggle %s on." (symbol-name name))
               (interactive)
               (unless (,wrapper-func-status) (,wrapper-func)))
             (defun ,wrapper-func-off ()
               ,(format "Toggle %s off." (symbol-name name))
               (interactive)
               (when (,wrapper-func-status) (,wrapper-func)))))
       ,@bindkeys)))

(provide 'jem-toggle)
