;; jem-log-lib.el

(defconst jem-log-max-size 1000
  "Maximum number of logs stored.")

(defvar jem--logs '()
  "List with log entries. A log entry is of the form '(TIME STRING).")

(defun jem-log (msg)
  "Adds MSG to logs."
  (interactive "sMessage: ")
  (let* ((log-size (length jem--logs))
         (timestamp (current-time)))
    (setq jem--logs (cons `(,timestamp ,msg) jem--logs))
    (if (> log-size jem-log-max-size)
        (setq jem--logs (butlast jem--logs (- log-size jem-log-max-size))))))

(provide 'jem-log-lib)
