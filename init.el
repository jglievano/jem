;; JEM
;; init.el - start

(defconst jem-version "1.0.0" "Jem version")
(defconst jem-emacs-min-version "24.3" "Minimum required Emacs version")
(defconst jem-start-time (current-time))

(if (version<= emacs-version jem-emacs-min-version)
    (message (concat "jem> Jem requires Emacs >= %s. "
                     "Your current Emacs version is %s.")
             jem-emacs-min-version emacs-version)

  (if (file-exists-p (expand-file-name "~/.custom.el"))
    (load "~/.custom.el"))

  (eval-and-compile
    (mapc #'(lambda (path)
              (add-to-list 'load-path
                           (expand-file-name path user-emacs-directory)))
          '("jem")))

  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)

  (require 'evil)
  (evil-mode 1)

  (require 'jem-lib)
  (jem-init)

  (if (and (fboundp 'server-running-p)
           (not (server-running-p)))
    (server-start)))
