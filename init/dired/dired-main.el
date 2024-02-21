(leaf dired
  :after window-extras
  :preface
  (autoload 'dired-get-filename "dired")
  (autoload 'term-set-escape-char "term")
  :require dired dired-extras daanturo-dired
  :hook ((dired-mode-hook . auto-revert-mode)
         (dired-mode-hook . dired-hide-details-mode))
  :bind (;; ("C-x ~" . daanturo-dired-home)
         ("C-x M-~" . my-dired-emacs)
         (:dired-mode-map :package window-extras
                          (("C-M-o" . xc/switch-to-last-window)))
         (:dired-mode-map :package dired-extras
                          (("RET" . xc/dired-find-file)))
         (:dired-mode-map :package daanturo-dired ;; conflicts with dired-open-file
                          (("r" . nil) ;; diredp-rename-this-file
                           ("r" . daanturo-dired-do-rename)))
         (:dired-mode-map
          ;; ("?") . daanturo-dired-delete-no-trash
          ((("C-c C-c" . compile)
            ("w" . wdired-change-to-wdired-mode)
            ("h" . diredp-dired-recent-dirs)
            ("$" . eshell)
            ;; previous eww-open-file but it prompts
            ("e" . browse-url-of-dired-file)
            ("E" . redguardtoo-ediff-files)
            ("@" . dired-run-command)
            ("l" . nil)
            ("l" . dired-up-directory)
            (";" . dired-next-subdir))))) ;; dired-do-redisplay
  :custom ((dired-listing-switches
            . "-afxhlv --group-directories-first --time-style=long-iso")
           (dired-free-space-args . "-ph")
           ;; which files not to display
           (dired-omit-files . "^\\.\\|^#.*#$")
           ;; which directories to track
           (dirtrack-list . '("^[^:]*:\\(?:\e\\[[0-9]+m\\)?\\([^$#\e]+\\)" 1))
           (ls-lisp-ignore-case . t)
           (ls-lisp-dirs-first . t)
           (dired-dwim-target . t)
           ;; use system's trash can
           (delete-by-moving-to-trash . t)
           ;; don’t delete excess backup versions silently
           (delete-old-versions . t)
           ;; don't hide symbolic link targets
           (wdired-allow-to-change-permissions . nil)
           (wdired-create-parent-directories . t)
           (dired-auto-revert-buffer . #'dired-directory-changed-p)
           (dired-recursive-deletes . 'always)
           (delete-by-moving-to-trash . t)
           (dired-always-read-filesystem . t)
           (dired-vc-rename-file . t)
           (dired-copy-preserve-time . t)
           (dired-recursive-copies . t)
           (dired-clean-confirm-killing-deleted-buffers . nil)
           (dired-kill-when-opening-new-dired-buffer . t)
           (dired-hide-details-hide-symlink-targets . nil)
           (dired-omit-verbose . nil)  ;; don't show messages when omitting files
           (dired-recursive-copies . 'always)  ;; always copy recursively
           (dired-recursive-deletes . 'always) ;; always delete recursively
           (find-ls-option . '("-print0 | xargs -p4 -0 ls -ldn" . "-ldn"))
           (find-ls-subdir-switches . "-ldn")
           (find-ls-subdir-switches . "-ldn")
           ;; run command depending on os, depending on file-type
           (dired-guess-shell-alist-user
            .
            `((,(rx "."
                    (or
                     ;; videos
                     "mp4" "avi" "mkv" "flv" "ogv" "ogg" "mov"
                     ;; music
                     "wav" "mp3" "flac"
                     ;; images
                     "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
                     ;; docs
                     "pdf" "md" "djvu" "ps" "eps" "doc" "docx" "xls" "xlsx"
                     "ppt" "pptx")
                    string-end)
               ,(pcase system-type
                  ('gnu/linux "xdg-open")
                  ('darwin "open")
                  ('windows-nt "start")
                  (_ ""))))))
  :config
  ;; always show dot-fil
  (setq dired-mode-hook (delq 'dired-omit-mode dired-mode-hook)))
