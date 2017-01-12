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

(defconst jem-themes '("blackboard"
                       "dracula"
                       "ujelly"
                       "zenburn")
  "List of themes available for Jem.")

(defconst jem-third-party-directory
  (expand-file-name (concat user-emacs-directory "third-party/"))
  "Path to jem third-party directory.")

(defvar jem--ivy-file-actions
  '(("f" find-file-other-frame "other frame")
    ("w" find-file-other-window "other window")
    ("v" jem-find-file-vsplit "in vertical split")
    ("s" jem-find-file-split "in horizontal split")
    ("l" find-file-literally "literally")
    ("d" jem-delete-file "delete file")
    ("r" jem-rename-file "rename file"))
  "Default ivy actions for files.")

(provide 'jem-definitions)
