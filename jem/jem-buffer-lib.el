;; jem-buffer-lib.el

(defconst jem-buffer-name "*jem*"
  "Name of `jem-buffer'.")

(defvar jem-buffer-status-line 0
  "Location of line where status displays in the dashboard.")

(defconst jem--path-to-banners (concat jem-directory "banners/")
  "Path to banners directory.")

(defun jem-append-to-dashboard (msg)
  "Appends msg to *jem*."
  )

(defun jem-show-dashboard ()
  "Switches to *jem* which works as a dashboard."
  (interactive)
  (if (not (get-buffer jem-buffer-name))
      (jem--create-dashboard))
  (switch-to-buffer jem-buffer-name))

(defun jem--create-dashboard ()
  "Creates *jem* buffer."
  (with-current-buffer (get-buffer-create jem-buffer-name)
    (jem--insert-banner-in-dashboard)))

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
