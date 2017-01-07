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

(if (version<= emacs-version jem-emacs-min-version)
    (message (concat "jem> Jem requires Emacs >= %s. "
                     "Your current Emacs version is %s.")
             jem-emacs-min-version emacs-version)

  (eval-and-compile
    (mapc #'(lambda (path)
              (add-to-list 'load-path
                           (expand-file-name path user-emacs-directory)))
          '("jem" "jem/libs" "third-party/use-package")))

  (require 'jem-lib)
  (jem-init))
