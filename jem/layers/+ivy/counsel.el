;; counsel-boot.el

(defun jem-ivy-counsel|init ()
  (use-package counsel
    :ensure t
    :config
    (define-key counsel-find-file-map (kbd "C-h") 'councel-up-directory)
    (jem-set-leader-keys
      "SPC" 'counsel-M-x
      "ff" 'counsel-find-file
      "fL" 'counsel-locate
      "?" 'counsel-descbinds
      "hdr" 'counsel-describe-function
      "hdm" 'jem-describe-mode
      "hdv" 'counsel-describe-variable
      "hR" 'jem-counsel-search-docs
      "iu" 'counsel-unicode-char
      "ry" 'counsel-yank-pop
      "sj" 'counsel-imenu
      "Ts" 'counsel-load-theme
      "/" 'jem-search-project-auto
      "*" 'jem-search-project-auto-region-or-symbol
      "sf" 'jem-search-auto
      "sF" 'jem-search-auto-region-or-symbol
      "sp" 'jem-search-project-auto
      "sP" 'jem-search-project-auto-region-or-symbol
      "saf" 'jem-search-ag
      "saF" 'jem-search-ag-region-or-symbol
      "sap" 'jem-search-project-ag
      "saP" 'jem-search-project-ag-region-or-symbol
      "stf" 'jem-search-pt
      "stF" 'jem-search-pt-region-or-symbol
      "stp" 'jem-search-project-pt
      "stP" 'jem-search-project-pt-region-or-symbol
      "sgf" 'jem-search-grep
      "sgF" 'jem-search-grep-region-or-symbol
      "sgp" 'counsel-git-grep
      "sgP" 'jem-counsel-git-grep-region-or-symbol
      "skf" 'jem-search-ack
      "skF" 'jem-search-ack-region-or-symbol
      "skp" 'jem-search-project-ack
      "skP" 'jem-search-project-ack-region-or-symbol)
    (ivy-set-actions 'counsel-find-file
                     jem--ivy-file-actions)
    (counsel-mode 1)
    (jem-hide-lighter counsel-mode)
    (ivy-set-display-transformer 'jem-counsel-search
                                 'counsel-git-grep-transformer)))
