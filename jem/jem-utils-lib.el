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

;; defvar & defconst.
;;
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

(defvar jem-really-kill-emacs nil
  "Prevent window manager close from closing instance.")

;; defadvice.
;;
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

;; defmacro.
;;
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

(defun jem-centered-buffer-mode-full-width ()
  "Center buffer in the frame."
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (jem-maximize-horizontally)
    (call-interactively 'jem-centered-buffer-mode)))

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

(defun jem-copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

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

(defun jem-dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun jem-ert-run-tests-buffer ()
  "Run all the tests in the current buffer."
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name))
  (let ((cbuf (current-buffer)))
    (ert '(satisfies (lambda (test)
                       (eq cbuf (jem--find-ert-test-buffer test)))))))

(defun jem-evil-goto-next-line-and-indent (&optional count)
  (interactive "p")
  (let ((counter (or count 1)))
    (while (> count 0)
      (join-line 1)
      (newline-and-indent)
      (setq counter (1- counter)))))

(defun jem-evil-insert-line-above (count)
  "Insert COUNT lines above the current line without changing the state."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun jem-evil-insert-line-below (count)
  "Insert COUNT lines below the current line without changing the state."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

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

(defun jem-find-user-init-file ()
  "Edit the `user-init-file' in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun jem-frame-killer ()
  "Kill server buffer and hide the main emacs window."
  (interactive)
  (condition-case-unless-debug nil
      (delete-frame nil 1)
    (error (make-frame-invisible nil 1))))

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

(defun jem-kill-emacs ()
  "Lose all changes and exit emacs."
  (interactive)
  (setq jem-really-kill-emacs t)
  (kill-emacs))

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

(defun jem-kill-other-buffers (&optional arg)
  "Kill all other buffers.

If the universal prefix argument is used then kill windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except '%s'? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "All other buffers deleted.")))

(defun jem-kill-this-buffer (&optional arg)
  "Kill the current buffer.

If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun jem-layout-double-columns ()
  "Set the layout to double columns."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun jem-layout-triple-columns ()
  "Set the layout to triple columns."
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun jem-linum-update-window-scale-fix (win)
  "Fix linum for scaled text in the window WIN."
  (set-window-margins win
                      (ceiling (* (if (boundp 'text-scale-mode-step)
                                      (expt text-scale-mode-step
                                            text-scale-mode-amount) 1)
                                  (if (car (window-margins))
                                      (car (window-margins)) 1)))))

(defun jem-maximize-horizontally ()
  "Delete all windows left or right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

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

(defun jem-new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun jem-no-linum (&rest ignore)
  "Disable linum in current buffer."
  (when (or 'linum-mode global-linum-mode)
    (linum-mode 0)))

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
  "Prompt to save changed buffers and exit emacs."
  (interactive)
  (setq jem-really-kill-emacs t)
  (save-some-buffers)
  (kill-emacs))

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

(defun jem-save-buffers-kill-emacs ()
  "Save all changed buffers and exit emacs."
  (interactive)
  (setq jem-really-kill-emacs t)
  (save-buffers-kill-emacs))

(defun jem-safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s? " (current-buffer)))
      (erase-buffer)))

(defun jem-save-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

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

(defun jem-show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun jem-sort-lines ()
  "Sort lines in region or current buffer."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (sort-lines nil beg end)))

(defun jem-split-and-new-line ()
  "Split a quoted string or s-expression and insert a new line with auto-indent."
  (interactive)
  (sp-split-sexp 1)
  (sp-newline))

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

(defun jem-toggle-centered-buffer-mode ()
  "Toggle `jem-centered-buffer-mode'."
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (call-interactively 'jem-centered-buffer-mode)))

(defun jem-toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(defun jem-toggle-frame-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (toggle-frame-fullscreen))

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

(defun jem-toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

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

(defun jem-unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system undecided-dos nil))

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

(defun jem--find-ert-test-buffer (ert-test)
  "Return the buffer where ERT-TEST is defined."
  (car (find-definition-noselect (ert-test-name ert-test) 'ert-deftest)))

(defun jem--layout-not-contains-buffer-p (buffer)
  "Return non-nil if current layout does not contain BUFFER."
  (not (persp-contain-buffer-p buffer)))

(defun jem--line-at-click ()
  "Determine the visual line at click."
  (save-excursion
    (let ((click-y (cddr (mouse-position)))
          (debug-on-error t)
          (line-move-visual t))
      (goto-char (window-start))
      (next-line (1- click-y))
      (1+ (line-number-at-pos)))))

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

(defun jem--run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (format "%S-local-vars-hook" major-mode))))

(provide 'jem-utils-lib)
