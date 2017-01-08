;; jem-lib.el

(require 'jem-definitions)

(defvar jem-layer-names '("root"))

(defun jem-activate-layer (layer-name)
  "Activates layer with LAYER-NAME."
  (jem-log (format "Activating %s layer." layer-name))
  (mapcar
   (lambda (filename)
     (if (and (not (string= filename "."))
              (not (string= filename "..")))
         (let* ((package-name (replace-regexp-in-string "-boot.el" "" filename)))
           (load (format "%s+%s/%s" jem-layers-directory layer-name filename))
           (funcall (intern (format "jem-%s-%s|init" layer-name package-name)))
           (jem-log (format "Loaded %s." package-name)))))
   (directory-files (concat jem-layers-directory "+" layer-name "/"))))

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

  ;; TODO: base is a nice to have. But should be optional.
  (jem-activate-layer "base")

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

(provide 'jem-lib)
