;; jem-buffer-lib.el

(defconst jem-dashboard-buffer-name "*jem*"
  "Name of dashboard's buffer.")

(defvar jem-log-size 10
  "Maximum number of logs displayed at a given time.")

(defvar jem--logs '()
  "List with logs entries. A log entry is a list too. e.g. '(time log).")

(defvar jem--dashboard-logs-start-line 0
  "Start line where logs display.")

(defconst jem--path-to-banners (concat jem-directory "banners/")
  "Path to banners directory.")

(defun jem-append (buffer-name msg)
  "Appends MSG to buffer with BUFFER-NAME."
  (with-current-buffer (get-buffer-create buffer-name)
    (goto-char (point-max))
    (insert msg)))

(defun jem-append-newline (buffer-name msg)
  "Appends MSG to buffer with BUFFER-NAME and adds a new line at the end."
  (jem-append buffer-name (concat msg "\n")))

(defun jem-create-centered-string (msg)
  "Creates a string from MSG that will show centered on a full line."
  (concat (make-string (floor (/ (- (window-width)
                                    (length msg)) 2)) ?\ )
          msg))

(defun jem-insert (buffer-name msg)
  "Inserts MSG to buffer with BUFFER-NAME at beginning."
  (with-current-buffer (get-buffer-create buffer-name)
    (save-excursion
      (goto-char (point-min))
      (delete-region (point) (progn (end-of-line) (point)))
      (insert msg))))

(defun jem-log (msg)
  "Appends MSG to dashboard output."
  (interactive "sMessage: ")
  (let* ((log-size (length jem--logs))
         (timestamp (format-time-string "%6N" (jem-elapsed-time))))
    (setq jem--logs (cons `(,timestamp ,msg) jem--logs))
    (if (> log-size jem-log-size)
        (setq jem--logs (butlast jem--logs (- log-size jem-log-size)))))
  (jem-refresh-dashboard))

(defun jem-refresh-dashboard ()
  "Refreshes dashboard."
  (jem--print-dashboard-logs jem--logs))

(defun jem-show-dashboard ()
  "Switches to *jem* which works as a dashboard."
  (interactive)
  (if (not (get-buffer jem-dashboard-buffer-name))
      (jem--create-dashboard))
  (switch-to-buffer jem-dashboard-buffer-name))

(defun jem-update-dashboard-status (msg)
  "Updates status line (first) with MSG."
  (jem-insert jem-dashboard-buffer-name
              (jem-create-centered-string (concat "[" msg "]"))))

(defun jem--create-dashboard ()
  "Creates *jem* buffer."
  (with-current-buffer (get-buffer-create jem-dashboard-buffer-name)
    ;; First line is the "status line".
    (jem-append jem-dashboard-buffer-name "\n")
    ;; Second line is the "info line".
    (jem-append-newline jem-dashboard-buffer-name
                        (jem-create-centered-string
                         (format "v%s @ emacs%s" jem-version emacs-version)))

    (jem--insert-banner-in-dashboard)
    (jem-append-newline jem-dashboard-buffer-name
                        (jem-create-centered-string "^^^"))
    (jem-update-dashboard-status "loaded")
    (setq jem--dashboard-logs-start-line
          (1+ (count-lines (point-min) (point-max))))
    ;; Create lines for log.
    (jem-append-newline jem-dashboard-buffer-name
                        (make-string jem-log-size ?\n))))

(defun jem--delete-line (point)
  "Deletes line where POINT is."
  (goto-char point)
  (let ((beg (point)))
    (forward-line 1)
    (forward-char -1)
    (delete-region beg (point))))

(defun jem--insert-banner-in-dashboard ()
  "Inserts banner in *jem*."
  (insert-string
   (with-temp-buffer
     (insert-file-contents (concat jem--path-to-banners
                                   "000-banner.txt"))
     (let ((banner-width 0))
       (while (not (eobp))
         (let ((line-length (- (line-end-position) (line-beginning-position))))
           (if (< banner-width line-length)
               (setq banner-width line-length)))
         (forward-line 1))
       (goto-char 0)
       (let ((margin (max 0 (floor (/ (- (window-width) banner-width) 2)))))
         (while (not (eobp))
           (insert (make-string margin ?\ ))
           (forward-line 1))))
     (buffer-string))))

(defun jem--print-dashboard-logs (logs)
  "Print logs in *jem*."
  (with-current-buffer (get-buffer-create jem-dashboard-buffer-name)
    (setq current-line 0)
    (while (and logs
                (< current-line jem-log-size))
      (setq log (car logs))
      (let* ((time (car log))
             (msg (car (cdr log))))
        (goto-line (+ jem--dashboard-logs-start-line current-line))
        (jem--delete-line (point))
        (insert (format "  (%s): %s" time msg)))
      (setq current-line (1+ current-line))
      (setq logs (cdr logs)))))

(provide 'jem-buffer-lib)
