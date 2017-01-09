;; jem-lib.el

(require 'jem-definitions)

(defvar jem-layer-names '("root"))
(defface jem-transient-state-title-face `((t :inherit mode-line))
  "Face for title of transient states.")

(defun jem-activate-layer (layer-name)
  "Activates layer with LAYER-NAME."
  (jem-log (format "Activating %s layer." layer-name))
  (mapcar
   (lambda (filename)
     (if (and (not (string= filename "."))
              (not (string= filename "..")))
         (let* ((package-name (replace-regexp-in-string "-boot.el" "" filename)))
           (load (format "%s+%s/%s" jem-layers-directory layer-name filename))
           (funcall (intern (format "jem-%s-%s|init" layer-name package-name))))))
   (directory-files (concat jem-layers-directory "+" layer-name "/"))))

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
                        (if ,hint-doc-p " " "\n"))
;                       ,hint-var
                       ',dyn-hint
                       (concat "\n" guide)))))
       ,@bindkeys)))

(defun jem-elapsed-time ()
  "Returns the elapsed time between `current-time' and `jem-start-time'."
  (time-subtract (current-time) jem-start-time))

(defmacro jem-hide-lighter (mode)
  "Diminish MODE name in mode line."
  `(eval-after-load 'diminish '(diminish ',mode)))

(defun jem-init ()
  "Initializes jem."
  ;; TODO: Write message in jem-buffer.
  (message "jem> Jem is initializing.")

  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (jem--redefine-base-buffers)
  (jem--simplify-default-ui)
  (jem--setup-backup-system)
  (jem--use-space-indentation)

  (require 'jem-buffer-lib)
  (jem-show-dashboard)

  (if (file-exists-p (expand-file-name "~/.custom.el"))
      (progn
        (jem-log "Loading .custom.el")
        (load "~/.custom.el")))

  (require 'use-package)
  (setq use-package-verbose init-file-debug
        use-package-inject-hooks t)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages")
                           ("elpa" . "https://elpa.org/packages/")))

  (require 'jem-keymap)
  (require 'jem-toggle)

  ;; Loading packages from +root is mandatory.
  (jem-activate-layer "root")

  ;; TODO: essentials is a nice to have. But should be optional.
  (jem-activate-layer "essentials")

  (if (and (fboundp 'server-running-p)
           (not (server-running-p)))
      (server-start))
  (jem-log "Started emacs server."))

(defun jem-mplist-get (plist prop)
  "Get the values associated to PROP in PLIST.

PLIST is a list where keys are keywords and values are all non-keyword elements
that follow it.

If there are multiple properties with the same keyword, only the first property
and its values are returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun jem-run-text-mode-hooks ()
  "Runs `text-mode-hook'. Usefulf or modes that don't derive from `text-mode'
but should."
  (run-hooks 'text-mode-hook))


(defun jem--redefine-base-buffers ()
  "Redefines configuration for base buffers such as *Messages* and *scratch*."
  ;; Specify how many lines to keep in *Messages*.
  (setq message-log-max 16384)

  ;; No splash screen.
  (setq inhibit-startup-message t)
  (setq initial-scratch-message ""))

(defun jem--setup-backup-system ()
  "Setups backup system."
  ;; Write backup files to their own directory.
  (setq backup-directory-alist
	`(("." . ,(expand-file-name
		   (concat user-emacs-directory "backups")))))

  ;; Make backup of files even when they're in version control.
  (setq vc-make-backup-files t))

(defun jem--simplify-default-ui ()
  "Removes unused Emacs UI."
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))

(defun jem--use-space-indentation ()
  "Some basic configuration to enforce space indentation.

The default space configuration for jem is 2 spaces."
  (setq-default indent-tabs-mode nil)
  (setq tab-width 2)
  (setq-default tab-always-indent 'complete))

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
  (intern (format "jem-%S-transient-state-body" name)))

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
  (intern (format "jem-%s-transient-state" name)))

(defun jem--transient-state-heads-name (name)
  "Return the name of the transient state heads variable."
  (intern (format "jem-%S-transient-state-heads" name)))

(defun jem--transient-state-make-doc (transient-state docstring &optional body)
  "Use `hydra' internal function to format and apply DOCSTRING."
  (let ((heads (jem--transient-state-heads-name transient-state)))
    (setq body (if body body '(nil nil :hint nil :foreign-keys nil)))
    (eval
     (hydra--format nil body docstring (symbol-value heads)))))

(defun jem--transient-state-props-var-name (name)
  "Return the name of the variable used to store the transient state properties."
  (intern (format "jem--%S-transient-state-props" name)))

(provide 'jem-lib)
