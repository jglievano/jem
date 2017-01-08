;; jem-definitions.

(defvar jem-default-jump-handlers '())

(defconst jem-directory
  (expand-file-name (concat user-emacs-directory "jem/"))
  "Path to jem directory.")

(defconst jem-cache-directory (concat jem-directory "cache/")
  "Path to jem cache directory.")

(defconst jem-emacs-leader-key "M-m"
  "Emacs leader key.")

(defconst jem-layers-directory
  (expand-file-name (concat jem-directory "layers/"))
  "Path to jem layers directory.")

(defconst jem-leader-key "SPC"
  "Evil leader key.")

(defconst jem-third-party-directory
  (expand-file-name (concat user-emacs-directory "third-party/"))
  "Path to jem third-party directory.")

(provide 'jem-definitions)
