;; company-boot.el

(defun jem-essentials-company|init ()
  (use-package company
    :ensure t
    :defer t
    :init
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil)
    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
  :config
  (defun jem--company-complete-common-or-cycle-backwards ()
    "Complete common prefix or cycle backwards."
    (interactive)
    (company-complete-common-or-cycle -1)))
;  (jem--auto-completion-set-RET-key-behavior 'company)
;  (jem--auto-completion-set-TAB-key-behavior 'company)
;  (jem--auto-completion-setup-key-sequence 'company)

;  (let ((map company-active-map))
;    (define-key map (kbd "C-/") 'company-search-candidates)
;    (define-key map (kbd "C-M-/") 'comapny-filter-candidates)
;    (define-key map (kbd "C-d") 'company-show-doc-buffer)))
