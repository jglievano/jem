;; keybindings-boot.el

(defvar jem-indent-sensitive-modes
  '(coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode
    yaml-mode)
  "Modes for which auto-indenting is suppressed.")

(defcustom jem-large-files-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode doc-view-mode
                 doc-view-mode-maybe ebrowse-tree-mode pdf-view-mode)
  "Major modes which `jem-check-large-file' will not be automatically applied
to."
  :group 'jem
  :type '(list symbol))

(defvar jem-linum-mdown-line nil
  "Define persistent variable for linum selection.")

(defvar jem-really-kill-emacs nil
  "Prevent window manager close from closing instance.")

(defcustom jem-yank-indent-threshold 1000
  "Threshold over which indentation does not automatically occur."
  :type 'number
  :group 'jem)

(defun jem-essentials-keybindings|init ())

(defun jem--run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (format "%S-local-vars-hook" major-mode))))

(defun jem-split-and-new-line ()
  "Split a quoted string or s-expression and insert a new line with auto-indent."
  (interactive)
  (sp-split-sexp 1)
  (sp-newline))

(defun jem-push-mark-and-goto-beginning-of-line ()
  "Push a mark at current location and go to the beginning of the line."
  (interactive)
  (push-mark (point))
  (evil-beginning-of-line))

(defun jem-push-mark-and-goto-end-of-line ()
  "Push a mark at current location and go to the end of the line."
  (interactive)
  (push-mark (point))
  (evil-end-of-line))

(defun jem-evil-insert-line-above (count)
  "Insert COUNT lines above the current line without changing the state."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun jem-evil-insert-line-below (count)
  "Insert COUNT lines below the current line without changing the state."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun jem-evil-goto-next-line-and-indent (&optional count)
  (interactive "p")
  (let ((counter (or count 1)))
    (while (> count 0)
      (join-line 1)
      (newline-and-indent)
      (setq counter (1- counter)))))

(defun jem-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode jem-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (evil-indent (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(defun jem-toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(defun jem-maximize-horizontally ()
  "Delete all windows left or right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun jem-toggle-centered-buffer-mode ()
  "Toggle `jem-centered-buffer-mode'."
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (call-interactively 'jem-centered-buffer-mode)))

(defun jem-centered-buffer-mode-full-width ()
  "Center buffer in the frame."
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (jem-maximize-horizontally)
    (call-interactively 'jem-centered-buffer-mode)))

(defun jem-useful-buffer-p (buffer)
  "Dtermines if a buffer is useful."
  (let ((buf-name (buffer-name buffer)))
    (or (with-current-buffer buffer
          (derived-mode-p 'comint-mode))
        (cl-loop for useful-regexp in jem-useful-buffers-regexp
                 thereis (string-match-p useful-regexp buf-name))
        (cl-loop for useless-regexp in jem-useless-buffers-regexp
                 never (string-match-p useless-regexp buf-name)))))

(defun jem-useless-buffer-p (buffer)
  "Determines if a buffer is useless."
  (not (jem-useful-buffer-p buffer)))

(defun jem-rotate-windows (count)
  "Rotate each window forwards. A negative prefix argument rotates each window
backwards. Dedicated windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))
                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))
                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))
                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

(defun jem-rotate-windows-backward (count)
  "Rotate each window backwards. Dedicated windows are left untouched."
  (interactive "p")
  (jem-rotate-windows (* -1 count)))

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
               (rename-file filename new-name i)
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

(defun jem-sudo-edit (&optional arg)
  (interactive "p")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname)
                                              last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))

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

(defun jem-delete-window (&optional arg)
  "Delete the current window.

If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

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

(defun jem-kill-this-buffer (&optional arg)
  "Kill the current buffer.

If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

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

(defun jem-kill-other-buffers (&optional arg)
  "Kill all other buffers.

If the universal prefix argument is used then kill windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except '%s'? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "All other buffers deleted.")))

(defun jem-toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(defun jem-show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun jem-find-user-init-file ()
  "Edit the `user-init-file' in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun jem-new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun jem-split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window))

(defun jem-split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun jem-layout-triple-columns ()
  "Set the layout to triple columns."
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun jem-layout-double-columns ()
  "Set the layout to double columns."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun jem-insert-line-above-no-indent (count)
  "Insert a new line above with no indentation."
  (interactive "p")
  (let ((p (+ (point) count)))
    (save-excursion
      (if (eq (line-number-at-pos) 1)
          (evil-move-beginning-of-line)
        (progn
          (evil-previous-line)
          (evil-move-end-of-line)))
      (while (> count 0)
        (insert "\n")
        (setq count (1- count))))
    (goto-char p)))

(defun jem-insert-line-below-no-indent (count)
  "Insert a new line below with no indentation."
  (interactive "p")
  (save-excursion
    (evil-move-end-of-line)
    (while (> count 0)
      (insert "\n")
      (setq count (1- count)))))

(defun jem-kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP. This function unlike
the build-in `kill-matching-buffers' does so without asking. The optional second
argument indicates whether to kill internal buffers. too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

(defadvice kill-emacs (around jem-really-exit activate)
  "Only kill emacs if a prefix is set."
  (if (and (not jem-really-kill-emacs)
           (jem--persistent-server-running-p))
      (jem-frame-killer)
    ad-do-it))

(defadvice save-buffers-kill-emacs (around jem-really-exit activate)
  "Only kill emacs if a prefix is set."
  (if jem-really-kill-emacs
      ad-do-it
    (jem-frame-killer)))

(defun jem-save-buffers-kill-emacs ()
  "Save all changed buffers and exit emacs."
  (interactive)
  (setq jem-really-kill-emacs t)
  (save-buffers-kill-emacs))

(defun jem-kill-emacs ()
  "Lose all changes and exit emacs."
  (interactive)
  (setq jem-really-kill-emacs t)
  (kill-emacs))

(defun jem-prompt-kill-emacs ()
  "Prompt to save changed buffers and exit emacs."
  (interactive)
  (setq jem-really-kill-emacs t)
  (save-some-buffers)
  (kill-emacs))

(defun jem-frame-killer ()
  "Kill server buffer and hide the main emacs window."
  (interactive)
  (condition-case-unless-debug nil
      (delete-frame nil 1)
    (error (make-frame-invisible nil 1))))

(defun jem-toggle-frame-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (toggle-frame-fullscreen))

(defun jem-toggle-fullscreen ()
  "Toggle full screen on X11 and Carbon."
  (interactive)
  (cond
   ((eq window-system 'x)
    (set-frame-parameter nil 'fullscreen
                         (when (not (frame-parameter nil 'fullscreen))
                           'fullboth)))
   ((eq window-system 'mac)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullscreen)))))

(defun jem-toggle-frame-fullscreen-non-native ()
  "Toggle full screen non-natively."
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
           (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
             (if (eq (frame-parameter nil 'maximized) 'maximized)
                 'maximized)
           'fullboth)))))

(defmacro jem-advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS. The body of the advice
is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (format "%S-%s" command advice-name))
                              activate)
                    ,@body))
               commands)))

(defun jem-save-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

(defun jem-safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s? " (current-buffer)))
      (erase-buffer)))

(defun jem--find-ert-test-buffer (ert-test)
  "Return the buffer where ERT-TEST is defined."
  (car (find-definition-noselect (ert-test-name ert-test) 'ert-deftest)))

(defun jem-ert-run-tests-buffer ()
  "Run all the tests in the current buffer."
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name))
  (let ((cbuf (current-buffer)))
    (ert '(satisfies (lambda (test)
                       (eq cbuf (jem--find-ert-test-buffer test)))))))

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

(defun jem-copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun jem-copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

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

(defmacro jem-create-align-repeat-x
    (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "jem-align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (jem-align-repeat start end ,regexp ,justify-right after)))))

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

(defun jem-dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun jem-unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system undecided-dos nil))

(defun jem-copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

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

(defun jem-sort-lines ()
  "Sort lines in region or current buffer."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (sort-lines nil beg end)))

(defun jem--line-at-click ()
  "Determine the visual line at click."
  (save-excursion
    (let ((click-y (cddr (mouse-position)))
          (debug-on-error t)
          (line-move-visual t))
      (goto-char (window-start))
      (next-line (1- click-y))
      (1+ (line-number-at-pos)))))

(defun jem-md-select-linum (event)
  "Set point as jem-linum-mdown-line."
  (interactive "e")
  (mouse-select-window event)
  (goto-line (jem--line-at-click))
  (set-mark (point))
  (setq jem-linum-mdown line (line-number-at-pos)))

(defun jem-mu-select-linum ()
  "Select code block between point and jem-linum-mdown-line."
  (interactive)
  (when jem-linum-mdown-line
    (let (mu-line)
      (setq mu-line (jem--line-at-click))
      (goto-line (max jem-linum-mdown-line mu-line))
      (set-mark (line-end-position))
      (goto-line (min jem-linum-mdown-line mu-line))
      (setq jem-linum-mdown-line nil))))

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

(defun jem-yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) jem-yank-indent-threshold)
      (indent-region beg end nil)))

(jem-advise-commands
 "indent" (yank yank-pop evil-paste-before evil-paste-after) around
 "If current mode is not one of jem-indent-sensitive-modes indent yanked text."
 (evil-start-undo-step)
 ad-do-it
 (if (and (not (equal '(4) (ad-get-arg 0)))
          (not (member major-mode jem-indent-sensitive-modes))
          (or (derived-mode-p 'prog-mode)
              (member major-mode jem-indent-sensitive-modes)))
     (let ((transient-mark-mode nil)
           (save-undo buffer-undo-list))
       (jem-yank-advised-indent-function (region-beginning)
                                         (region-end)))))

(defun jem--display-in-split (buffer alist)
  "Split selected window and display BUFFER in the new window.

BUFFER and ALIST have the same form as in `display-buffer'. If ALIST contains a
split-side entry then its value must be usable as the SIDE argument for
`split-window'."
  (let ((window (split-window nil nil (cdr (assq 'split-side alist)))))
    (window--display-buffer buffer window 'window alist) window))

(defun jem-find-file-vsplit (file)
  "Find file in vertical split."
  (interactive "FFind file (vsplit): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(jem--display-in-split (split-side . right)))))

(defun jem-find-file-split (file)
  "Find file in horizontal split."
  (interactive "FFind file (split): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(jem--display-in-split (split-side . below)))))

(defun jem-switch-to-scratch-buffer ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun jem-close-compilation-window ()
  "Close the *compilation* buffer."
  (interactive)
  (when compilation-last-buffer
    (delete-windows-on compilation-last-buffer)))

(defun jem-no-linum (&rest ignore)
  "Disable linum in current buffer."
  (when (or 'linum-mode global-linum-mode)
    (linum-mode 0)))

(defun jem-linum-update-window-scale-fix (win)
  "Fix linum for scaled text in the window WIN."
  (set-window-margins win
                      (ceiling (* (if (boundp 'text-scale-mode-step)
                                      (expt text-scale-mode-step
                                            text-scale-mode-amount) 1)
                                  (if (car (window-margins))
                                      (car (window-margins)) 1)))))

(defun jem--persistent-server-running-p ()
  "Requires jem-really-kill-emacs to be toggled."
  (and (fboundp 'server-runnint-p)
       (server-running-p)))

(setq jem-keybinding-prefixes '(("a" "applications")
                                ("b" "buffers")
                                ("c" "compile/comments")
                                ("e" "errors")
                                ("f" "files")
                                ("fC" "files/convert")
                                ("fv" "variables")
                                ("h" "help")
                                ("hd" "help-describe")
                                ("i" "insertion")
                                ("j" "jump/join/split")
                                ("n" "narrow/numbers")
                                ("p" "projects")
                                ("p$" "projects/shell")
                                ("q" "quit")
                                ("t" "toggles")
                                ("T" "UI toggles")
                                ("w" "windows")
                                ("x" "text")
                                ("xa" "align")
                                ("xd" "delete")
                                ("xl" "lines")
                                ("xm" "move")
                                ("xt" "transpose")
                                ("xw" "words")))

(mapc (lambda (x) (apply #'jem-declare-prefix x))
      jem-keybinding-prefixes)

;; instantly display current keystrokes in mini-buffer.
(setq echo-keystrokes 0.02)
;; auto-indent on RET.
(define-key global-map (kbd "RET") 'newline-and-indent)

;; alternative to search next.
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
;; escape from isearch-mode.
(define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

;; escape.
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

;; universal argument.
(jem-set-leader-keys "u" 'universal-argument)
(define-key universal-argument-map
  (kbd (concat jem-leader-key " u")) 'universal-argument-more)

;; shell.
(jem-set-leader-keys "!" 'shell-command)

;; applications.
(jem-set-leader-keys
  "ac" 'calc-dispatch
  "ap" 'list-processes
  "aP" 'proced
  "au" 'undo-tree-visualize)

;; buffers.
(jem-set-leader-keys
  "TAB" 'jem-alternate-buffers
  "bd" 'jem-kill-this-buffer
  "be" 'jem-safe-erase-buffer
  "bh" 'jem-home
  "b C-d" 'jem-kill-matching-buffers-rudely
  "bn" 'next-buffer
  "bm" 'jem-kill-other-buffers
  "bN" 'jem-new-empty-buffer
  "bP" 'jem-copy-clipboard-to-whole-buffer
  "bp" 'previous-buffer
  "bR" 'jem-safe-revert-buffer
  "bs" 'jem-switch-to-scratch-buffer
  "bY" 'jem-copy-whole-buffer-to-clipboard
  "bw" 'read-only-mode)

;; errors.
(jem-set-leader-keys
  "en" 'jem-next-error
  "eN" 'jem-previous-error
  "ep" 'jem-previous-error)

(jem-define-transient-state error
  :title "Error transient state"
  :hint-is-doc t
  :dynamic-hint
  (let ((sys (jem-error-delegate)))
    (cond
     ((eq 'flycheck sys)
      "\nBrowsing flycheck errors from this buffer.")
     ((eq 'emacs sys)
      (let ((buf (next-error-find-buffer)))
        (if buf
            (concat "\nBrowsing entries from \""
                    (buffer-name buf)
                    "\""
                    (with-current-buffer buf
                      (when jem--gne-line-func
                        (format " (%d of %d)"
                                (max 1 (1+ (- jem--gne-cur-line
                                              jem--gne-min-line)))
                                (1+ (- jem--gne-max-line
                                       jem--gne-min-line))))))
          "\nNo next-error capable buffer found.")))))
  :bindings
  ("n" jem-next-error "next")
  ("p" jem-previous-error "prev")
  ("q" nil "quit" :exit t)
  :evil-leader "e.")

;; file.
(jem-set-leader-keys
  "fc" 'jem-copy-file
  "fD" 'jem-delete-current-buffer-file
  "fei" 'jem-find-user-init-file
  "fed" 'jem-find-dotfile
  "feD" 'jem-ediff-dotfile-and-template
  "fev" 'jem-display-and-copy-version
  "fCd" 'jem-unix2dos
  "fCu" 'jem-dos2unix
  "fg" 'rgrep
  "fl" 'find-file-literally
  "fE" 'jem-sudo-edit
  "fo" 'jem-open-file-or-directory-in-external-app
  "fR" 'jem-rename-current-buffer-file
  "fS" 'evil-write-all
  "fs" 'save-buffer
  "fvd" 'add-dir-local-variable
  "fvf" 'add-file-local-variable
  "fvp" 'add-file-local-variable-prop-line
  "fy" 'jem-show-and-copy-buffer-filename)

;; help.
(jem-set-leader-keys
  "hdb" 'describe-bindings
  "hdc" 'describe-char
  "hdf" 'describe-function
  "hdk" 'describe-key
  "hdl" 'jem-describe-last-keys
  "hdp" 'describe-package
  "hds" 'jem-describe-system-info
  "hdt" 'describe-theme
  "hdv" 'describe-variable
  "hn" 'view-emacs-news)

;; insert.
(jem-set-leader-keys
  "iJ" 'jem-insert-line-below-no-indent
  "iK" 'jem-insert-line-above-no-indent
  "ik" 'jem-evil-insert-line-above
  "ij" 'jem-evil-insert-line-below)

;; format
(jem-set-leader-keys
  "jo" 'open-line
  "j=" 'jem-indent-region-or-buffer
  "jS" 'jem-split-and-new-line
  "jk" 'jem-evil-goto-next-line-and-indent)

;; navigation.
(jem-set-leader-keys
  "j0" 'jem-push-mark-and-goto-beginning-of-line
  "j$" 'jem-push-mark-and-goto-end-of-line
  "jf" 'find-function
  "jv" 'find-variable)

;; compilation.
(jem-set-leader-keys
  "cC" 'compile
  "ck" 'kill-compilation
  "cr" 'recompile
  "cd" 'jem-close-compilation-window)

(with-eval-after-load 'compile
  (define-key compilation-mode-map "r" 'recompile)
  (define-key compilation-mode-map "g" nil))

;; narrow and widen.
(jem-set-leader-keys
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen)

;; toggle.
(jem-add-toggle highlight-current-line-globally
  :mode global-hl-line-mode
  :documentation "Globally highlight the current line."
  :evil-leader "thh")
(jem-add-toggle truncate-lines
  :status truncate-lines
  :on (toggle-truncate-lines)
  :off (toggle-truncate-lines -1)
  :documentation "Truncate long lines (no wrap)."
  :evil-leader "tl")
(jem-add-toggle visual-line-navigation
  :status visual-line-mode
  :on
  (progn
    (visual-line-mode)
    (evil-define-minor-mode-key 'motion 'visual-line-mode "j"
      'evil-next-visual-line)
    (evil-define-minor-mode-key 'motion 'visual-line-mode "k"
      'evil-previous-visual-line)
    (when (bound-and-true-p evil-escape-mode)
      (evil-escape-mode -1)
      (setq evil-escape-motion-state-shadowed-func nil)
      (evil-define-minor-mode-key 'motion 'visual-line-mode "j"
        'evil-next-visual-line)
      (evil-define-minor-mode-key 'motion 'visual-line-mode "k"
        'evil-previous-visual-line)
      (evil-escape-mode))
    (evil-normalize-keymaps))
  :off
  (progn
    (visual-line-mode -1)
    (evil-normalize-keymaps))
  :documentation "Move point according to visual lines."
  :evil-leader "tL")
(jem-add-toggle auto-fill-mode
  :status auto-fill-function
  :on (auto-fill-mode)
  :off (auto-fill-mode -1)
  :documentation "Break line beyong `current-fill-column' while editing."
  :evil-leader "tF")
(jem-add-toggle debug-on-error
  :status debug-on-error
  :on (setq debug-on-error t)
  :off (setq debug-on-error nil)
  :documentation "Toggle display of backtrace when an error happens."
  :evil-leader "Tf")
(jem-add-toggle fringe
  :status (not (equal fringe-mode 0))
  :on (call-interactively 'fringe-mode)
  :off (fringe-mode 0)
  :documentation "Display the fringe in GUI mode."
  :evil-leader "Tf")
(jem-add-toggle fullscreen-frame
  :status (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
  :on (jem-toggle-frame-fullscreen)
  :off (jem-toggle-frame-fullscreen)
  :documentation "Display the current frame in full screen."
  :evil-leader "TF")
(jem-add-toggle maximize-frame
  :status (eq (frame-parameter nil 'fullscreen) 'maximized)
  :on (toggle-frame-maximized)
  :off (toggle-frame-maximized)
  :documentation "Maximize the current frame."
  :evil-leader "TM")
(jem-add-toggle semantic-stickyfunc
  :mode semantic-stickyfunc-mode
  :documentation "Enable semantic-stickyfunc."
  :evil-leader "TS")
(jem-add-toggle semantic-stickyfunc-globally
  :mode global-semantic-stickyfunc-mode
  :documentation "Enable semantic-stickyfunc globally."
  :evil-leader "T C-S")

;; quit.
(jem-set-leader-keys
  "qs" 'jem-save-buffers-kill-emacs
  "qq" 'jem-prompt-kill-emacs
  "qQ" 'jem-kill-emacs
  "qz" 'jem-frame-killer)

;; window.
(defun split-window-below-and-focus ()
  "Split the window vertically and focus."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (goldern-ratio)))
(defun split-window-right-and-focus ()
  "Split the window horizontally and focus."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))
(jem-set-leader-keys
  "w2" 'jem-layout-double-columns
  "w3" 'jem-layout-triple-columns
  "wb" 'jem-switch-to-minibuffer-window
  "wd" 'jem-delete-window
  "wt" 'jem-toggle-current-window-dedication
  "wf" 'follow-mode
  "wF" 'make-frame
  "wH" 'evil-window-move-far-left
  "w <S-left>" 'evil-window-move-far-left
  "wh" 'evil-window-left
  "w <left>" 'evil-window-left
  "wJ" 'evil-window-move-very-bottom
  "w <S-down>" 'evil-window-move-very-bottom
  "wj" 'evil-window-down
  "w <down>" 'evil-window-down
  "wK" 'evil-window-move-very-top
  "w <S-up>" 'evil-window-move-very-top
  "wk" 'evil-window-up
  "w <up>" 'evil-window-up
  "wL" 'evil-window-move-far-right
  "w <S-right>" 'evil-window-move-far-right
  "wl" 'evil-window-right
  "w <right>" 'evil-window-right
  "wm" 'jem-toggle-maximize-buffer
  "wc" 'jem-toggle-centered-buffer-mode
  "wC" 'jem-centered-buffer-mode-full-width
  "wo" 'other-frame
  "wr" 'jem-rotate-windows
  "wR" 'jem-rotate-windows-backward
  "ws" 'split-window-below
  "wS" 'split-window-below-and-focus
  "w-" 'split-window-below
  "wU" 'winner-redo
  "wu" 'winner-undo
  "wv" 'split-window-right
  "wV" 'split-window-right-and-focus
  "ww" 'other-window
  "w/" 'split-window-right
  "w=" 'balance-window
  "w_" 'jem-maximize-horizontally)

;; text.
(defalias 'count-region 'count-words-region)
(jem-set-leader-keys
  "xa&" 'jem-align-repeat-ampersand
  "xa(" 'jem-align-repeat-left-paren
  "xa)" 'jem-align-repeat-right-paren
  "xa," 'jem-align-repeat-comma
  "xa." 'jem-align-repeat-decimal
  "xa:" 'jem-align-repeat-colon
  "xa;" 'jem-align-repeat-semicolon
  "xa=" 'jem-align-repeat-equal
  "xa\\" 'jem-align-repeat-backslash
  "xaa" 'align
  "xac" 'align-current
  "xam" 'jem-align-repeat-math-oper
  "xar" 'jem-align-repeat
  "xa|" 'jem-align-repeat-bar
  "xc" 'count-region
  "xdw" 'delete-trailing-whitespace
  "xjc" 'set-justification-center
  "xjf" 'set-justification-full
  "xjl" 'set-justification-left
  "xjn" 'set-justification-none
  "xjr" 'set-justification-right
  "xls" 'jem-sort-lines
  "xlu" 'jem-uniquify-lines
  "xtc" 'transpose-chars
  "xtl" 'transpose-lines
  "xtw" 'transpose-words
  "xU" 'upcase-region
  "xu" 'downcase-region
  "x TAB" 'indent-rigidly)
(define-key indent-rigidly-map "h" 'indent-rigidly-left)
(define-key indent-rigidly-map "l" 'indent-rigidly-right)
(define-key indent-rigidly-map "H" 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map "L" 'indent-rigidly-right-to-tab-stop)

;; shell.
(with-eval-after-load 'shell
  (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
  (evil-define-key 'insert comint-mode-map [down] 'comint-next-input))

;; buffer transient state.
(jem-define-transient-state buffer
  :title "Buffer selection transient state"
  :bindings
  ("n" next-buffer "next")
  ("N" previous-buffer "previous")
  ("p" previous-buffer "previous")
  ("K" jem-kill-this-buffer "kill")
  ("q" nil "quit" :exit t))
(jem-set-leader-keys "b." 'jem-buffer-transient-state/body)

;; window manipulation transient state.
(defun jem-shrink-window-horizontally (delta)
  (interactive "p")
  (shrink-window delta t))
(defun jem-shrink-window (delta)
  (interactive "p")
  (shrink-window delta t))
(defun jem-enlarge-window (delta)
  (interactive "p")
  (enlarge-window delta))
(defun jem-enlarge-window-horizontally (delta)
  (interactive "p")
  (enlarge-window delta t))
(jem-define-transient-state window-manipulation
  :title "Window manipulation transient state"
  :doc "
 Select^^^^              Move^^^^              Split^^                Resize^^                     Other^^
 ──────^^^^───────────── ────^^^^───────────── ─────^^─────────────── ──────^^──────────────────── ─────^^──────────────────────────────
 [_j_/_k_] down/up       [_J_/_K_] down/up     [_s_] vertical         [_[_] shrink horizontally    [_q_] quit
 [_h_/_l_] left/right    [_H_/_L_] left/right  [_S_] vert & follow    [_]_] enlarge horizontally   [_u_] restore prev layout
 [_0_-_9_] window N      [_r_]^^   rotate fwd  [_v_] horizontal       [_{_] shrink vertically      [_U_] restore next layout
 [_w_]^^   other window  [_R_]^^   rotate bwd  [_V_] horiz & follow   [_}_] enlarge vertically     [_d_] close current
 [_o_]^^   other frame   ^^^^                  ^^                     ^^                           [_D_] close other"
  :bindings
  ("q" nil :exit t)
  ("0" select-window-0)
  ("1" select-window-1)
  ("2" select-window-2)
  ("3" select-window-3)
  ("4" select-window-4)
  ("5" select-window-5)
  ("6" select-window-6)
  ("7" select-window-7)
  ("8" select-window-8)
  ("9" select-window-9)
  ("-" split-window-below-and-focus)
  ("/" split-window-right-and-focus)
  ("[" jem-shrink-window-horizontally)
  ("]" jem-enlarge-window-horizontally)
  ("{" jem-shrink-window)
  ("}" jem-enlarge-window)
  ("d" delete-window)
  ("D" delete-other-windows)
  ("h" evil-window-left)
  ("<left>" evil-window-left)
  ("j" evil-window-down)
  ("<down>" evil-window-down)
  ("k" evil-window-up)
  ("<up>" evil-window-up)
  ("l" evil-window-right)
  ("<right>" evil-window-right)
  ("H" evil-window-move-far-left)
  ("<S-left>" evil-window-move-far-left)
  ("J" evil-window-move-very-bottom)
  ("<S-down>" evil-window-move-very-bottom)
  ("K" evil-window-move-very-top)
  ("<S-up>" evil-window-move-very-top)
  ("L" evil-window-move-far-right)
  ("<S-right>" evil-window-move-far-right)
  ("o" other-frame)
  ("r" jem-rotate-windows)
  ("R" jem-rotate-windows-backward)
  ("s" split-window-below)
  ("S" split-window-below-and-focus)
  ("u" winner-undo)
  ("U" winner-redo)
  ("v" split-window-right)
  ("V" split-window-right-and-focus)
  ("w" other-window))
(jem-set-leader-keys "w."
  'jem-window-manipulation-transient-state/body)
