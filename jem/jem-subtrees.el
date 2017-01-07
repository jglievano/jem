;; jem-subtrees.el

(defconst jem-subtrees-list
  '("jwiegley/use-package"))

(defun jem-install-subtrees ()
  "Install all subtrees in `jem-subrees-list'."
  (mapcar (lambda (subtree)
            (let* ((url (format "git@github.com:%s.git" subtree))
                   (path (concat jem-third-party-directory subtree))
                   (default-directory user-emacs-directory))
              (if (not (file-exists-p path))
                  (progn
                    (shell-command (format "git remote add -f %s %s"
                                           subtree url))
                    (shell-command
                     (format "git subtree add --prefix %s %s master --squash"
                             path subtree)))))) jem-subtrees-list))

(provide 'jem-subtrees)
