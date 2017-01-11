;; jem-utils-lib.el

;; defcustom.
;;
(defcustom jem-large-files-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode doc-view-mode
                 doc-view-mode-maybe ebrowse-tree-mode pdf-view-mode)
  "Major modes which `jem-check-large-file' will not be automatically applied
to."
  :group 'jem
  :type '(list symbol))

;; defmacro.
;;
(defmacro jem-create-align-repeat-x
    (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "jem-align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (jem-align-repeat start end ,regexp ,justify-right after)))))

;; defun.
;;
(defun jem-ace-delete-window (&optional arg)
  "Ace delete window.

If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (require 'ace-window)
  (aw-select
   " Ace - Delete Window"
   (lambda (window)
     (when (equal '(4) arg)
       (with-selected-window window
         (jem-kill-this-buffer arg)))
     (aw-delete-window window))))

(defun jem-ace-kill-this-buffer (&optional arg)
  "Ace kill visibile buffer in a window.

If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (require 'ace-window)
  (let (golden-ratio-mode)
    (aw-select
     " Ace - Kill buffer in Window"
     (lambda (window)
       (with-selected-window window
         (jem-kill-this-buffer arg))))))

(defun jem-align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression. If
JUSTIFY-RIGHT is non-nil then justify to the right instead of the left. If AFTER
is non-nil then add whitespace to the left instead of the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))
    (message "%S" complete-regexp)
    (align-regexp start end complete-regexp group 1 t)))

(jem-create-align-repeat-x "comma" "," nil t)
(jem-create-align-repeat-x "semicolon" ";" nil t)
(jem-create-align-repeat-x "colon" ":" nil t)
(jem-create-align-repeat-x "equal" "=")
(jem-create-align-repeat-x "math-oper" "[+\\-*/]")
(jem-create-align-repeat-x "ampersand" "&")
(jem-create-align-repeat-x "bar" "|")
(jem-create-align-repeat-x "left-paren" "(")
(jem-create-align-repeat-x "right-paren" ")" t)
(jem-create-align-repeat-x "backslash" "\\\\")

(defun jem-align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs."
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defun jem-check-large-file ()
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and
           (not (memq major-mode jem-large-file-modes-list))
           size (> size (* 1024 1024 jem-large-file-size))
           (y-or-n-p (format (concat "%s is a large file, open literally to "
                                     "avoid performance issues?")
                             filename)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(defun jem-close-compilation-window ()
  "Close the *compilation* buffer."
  (interactive)
  (when compilation-last-buffer
    (delete-windows-on compilation-last-buffer)))

(defun jem-copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun jem-copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun jem-delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to DELETE? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (projectile-project-p)
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' removed." filename)))))

(defun jem-delete-file (filename &optional ask-user)
  "Remove specified file or directory.

Also kills associated buffer and invalidates projectile cache when possible.

When ASK-USER is non-nil then the user will be asked to confirm."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to DELETE? "))
      (delete-file filename)
      (when (projectile-project-p)
        (call-interactively #'projectile-invalidate-cache)))))

(defun jem-delete-window (&optional arg)
  "Delete the current window.

If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(defun jem-find-file-split (file)
  "Find file in horizontal split."
  (interactive "FFind file (split): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(jem--display-in-split (split-side . below)))))

(defun jem-find-file-vsplit (file)
  "Find file in vertical split."
  (interactive "FFind file (vsplit): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(jem--display-in-split (split-side . right)))))

(defun jem-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (evil-indent (point-min) (point-max))
        (message "Indented buffer.")))
    (whitespace-cleanup)))

(defun jem-new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun jem-open-file-or-directory-in-external-app (arg)
  "Open current file in external application. If the universal prefix argument
is used then open the folder containing the current file by the default
explorer."
  (interactive "P")
  (if arg
      (jem--open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (jem--open-in-external-app file-path)
        (message "No file associated to this buffer.")))))

(defun jem-prompt-kill-emacs ()
  "Prompt to save buffers and exit Emacs."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun jem-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir (file-name-directory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file." name)
      (let ((new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists." new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p
                                                       (format
                                                        "Create directory '%s'?"
                                                        dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (when (projectile-project-p)
                 (call-interactively #'projectile-invalidate-cache))
               (message "File '%s' renamed to '%s'" name
                        (file-name-nondirectory new-name))))))))

(defun jem-rename-file (filename &optional new-filename)
  "Rename FILENAME to NEW-FILENAME.

When NEW-FILENAME is not specified, asks user for new. Also renames associated
buffer (if exists), invalidates projectile cache when it's possible and update
recentf list."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let* ((buffer (find-buffer-visiting filename))
           (short-name (file-name-nondirectory filename))
           (new-name (if new-filename new-filename
                       (read-file-name
                        (format "Rename %s to: " short-name)))))
      (cond ((get-buffer new-name)
             (error "A buffer named '%s' already exists." new-name))
            (t
             (let ((dir (file-name-directory new-name)))
               (when (and (not (file-exists-p dir)) (yew-or-no-p
                                                     (format
                                                      "Create directory '%s'?"
                                                      dir)))
                 (make-directory dir t)))
             (rename-file filename new-name 1)
             (when buffer (kill-buffer buffer)
                   (find-file new-name))
             (when (fboundp 'recentf-add-file)
               (recentf-add-file new-name)
               (recentf-remove-if-non-kept filename))
             (when (projectile-project-p)
               (call-interactively #'projectile-invalidate-cache))
             (message "File '%s' renamed to '%s'" short-name
                     (file-name-nondirectory new-name)))))))

(defun jem-select-current-block ()
  "Select the current block of text between blank lines."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p2 (point))))
    (set-mark p1)))

(defun jem-sort-lines ()
  "Sort lines in region or current buffer."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (sort-lines nil beg end)))

(defun jem-split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun jem-split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun jem-switch-to-scratch-buffer ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun jem-uniquify-lines ()
  "Remove duplicate adjacent lines in region or current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((beg (if (region-active-p) (region-beginning) (point-min)))
            (end (if (region-active-p) (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(defun jem--display-in-split (buffer alist)
  "Split selected window and display BUFFER in the new window.

BUFFER and ALIST have the same form as in `display-buffer'. If ALIST contains a
split-side entry then its value must be usable as the SIDE argument for
`split-window'."
  (let ((window (split-window nil nil (cdr (assq 'split-side alist)))))
    (window--display-buffer buffer window 'window alist) window))

(defun jem--open-in-external-app (file-path)
  "Open `file-path' in external application."
  (cond
   ((eq system-type 'windows-nt) (w32-shell-execute "open"
                                                    (replace-regexp-in-string
                                                     "/"
                                                     "\\\\"
                                                     file-path)))
   ((eq system-type 'darwin) (shell-command (format "open \"%s\"" file-path)))
   ((eq system-type 'gnu/linux) (let ((process-connection-type nil))
                                  (start-process "" nil "xdg-open"
                                                 file-path)))))

(defun jem--persistent-server-running-p ()
  "Requires jem-really-kill-emacs to be toggled."
  (and (fboundp 'server-running-p)
       (server-running-p)))

(provide 'jem-utils-lib)
