;; winner-boot.el

(defun jem-essentials-winner|init ()
  (use-package winner
    :ensure t
    :init
    (winner-mode t)
    (setq jem-winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*"))
    (setq winner-boring-buffers
          (append winner-boring-buffers jem-winner-boring-buffers))
    (winner-mode t)))
