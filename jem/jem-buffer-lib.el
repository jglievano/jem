;; jem-buffer-lib.el

(defconst jem-dashboard-buffer-name "*jem*"
  "Name of dashboard's buffer.")

(defconst jem--path-to-banners (concat jem-directory "banners/")
  "Path to banners directory.")

(defun jem-add-log-to-dashboard (log)
  "Appends LOG to dashboard output."
  (jem-append-newline jem-dashboard-buffer-name
                      (format "  (%s): %s"
                              (format-time-string "%6N" (jem-elapsed-time))
                              log)))

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

(defun jem-show-dashboard ()
  "Switches to *jem* which works as a dashboard."
  (interactive)
  (if (not (get-buffer jem-dashboard-buffer-name))
      (jem--create-dashboard))
  (switch-to-buffer jem-dashboard-buffer-name))

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
                        (jem-create-centered-string "---"))
    (jem-update-dashboard-status "loaded")))

(defun jem-update-dashboard-status (msg)
  "Updates status line (first) with MSG."
  (jem-insert jem-dashboard-buffer-name
              (jem-create-centered-string (concat "[" msg "]"))))

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

(provide 'jem-buffer-lib)
