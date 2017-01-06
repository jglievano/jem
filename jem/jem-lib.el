;; jem-lib.el

(require 'jem-definitions)

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
  (jem-show-dashboard))

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
