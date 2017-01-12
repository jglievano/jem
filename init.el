;; JEM
;; init.el - start


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst jem-version "1.0.0" "Jem version")
(defconst jem-emacs-min-version "24.3" "Minimum required Emacs version")
(defconst jem-start-time (current-time))

(defconst jem-load-paths '("jem"
                           "jem/libs"
                           "third-party"
                           "third-party/use-package"))

(if (version<= emacs-version jem-emacs-min-version)
    (message (concat "jem> Jem requires Emacs >= %s. "
                     "Your current Emacs version is %s.")
             jem-emacs-min-version emacs-version)

  (eval-and-compile
    (mapc #'(lambda (path)
              (add-to-list 'load-path
                           (expand-file-name path user-emacs-directory)))
          jem-load-paths))
  (add-to-list 'custom-theme-load-path (expand-file-name "third-party/themes"
                                                         user-emacs-directory))

  (require 'jem-lib)
  (jem-init)
  (load-theme 'zenburn))

;; end-of-file.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c0e2640af3eac7ee8d0dd084f26ee639e8ce955b7c784392a24310d242baa5d6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
