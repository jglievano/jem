;; auto-highlight-symbol-boot.el

(defun jem-ivy-auto-highlight-symbol|init ()
  ;; TODO: pre-init
  (jem-use-package-add-hook auto-highlight-symbol
    :post-init
    (setq jem--symbol-highlight-transient-state-doc
          (concat jem--symbol-highlight-transient-state-doc
                  "  [_b_] search buffers[_/_] search proj [_f_] search files")
          jem-symbol-highlight-transient-state-add-bindings
          '(("/" jem-search-project-auto-region-or-symbol :exit t)
            ("b" jem-swiper-all-region-or-symbol :exit t)
            ("f" jem-search-auto-region-or-symbol :exit t)))))
