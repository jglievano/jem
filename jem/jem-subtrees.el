;; jem-subtrees.el

(defconst jem-subtrees-list
  '("jwiegley/use-package"))

(defun jem-install-subtrees ()
  "Install all subtrees in `jem-subrees-list'."
  (mapcar (lambda (subtree)
            (let* ((package-name (replace-regexp-in-string ".+/" "" subtree))
                   (url (format "git@github.com:%s.git" subtree))
                   (path (concat jem-third-party-directory package-name))
                   (default-directory user-emacs-directory))
              (if (not (file-exists-p path))
                  (progn
                    (jem-log (format "git remote add -f %s %s" subtree url))
                    (shell-command (format "git remote add -f %s %s"
                                           subtree url))
                    (jem-log (format
                              "git subtree add --prefix %s %s master --squash"
                              path subtree))
                    (shell-command
                     (format "git subtree add --prefix third-party/%s %s master --squash"
                             package-name subtree)))))) jem-subtrees-list))

(provide 'jem-subtrees)
