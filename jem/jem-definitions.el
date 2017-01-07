;; jem-definitions.

(defconst jem-directory
  (expand-file-name (concat user-emacs-directory "jem/"))
  "Path to jem directory.")

(defconst jem-layers-directory
  (expand-file-name (concat jem-directory "layers/"))
  "Path to jem layers directory.")

(defconst jem-third-party-directory
  (expand-file-name (concat user-emacs-directory "third-party/"))
  "Path to jem third-party directory.")

(provide 'jem-definitions)
