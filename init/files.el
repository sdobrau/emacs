;; Useful and fast key-bindings for common operations.
;; Should convert some of these to org-capture at some point.

(leaf files-extras
  :bind (("C-x C-M-f" . find-file-rec)
         ("C-x s-f" . open-all-files-in-directory)
         ("C-x f" . nil) ;; set-fill-column
         ("C-x f r" . redguardtoo-vc-rename-file-and-buffer)
         ("C-x f d" . +delete-current-file)
         ("C-x f c" . redguardtoo-vc-copy-file-and-rename-buffer)
         ("C-x f t" . my-edit-todo-org-file)
         ("C-x f w" . jf/nab-file-name-to-clipboard)
         ("C-x f -" . xah-open-file-from-clipboard)
         ("C-x f i" . my-edit-init-org-file)
         ("C-x f u" . revert-buffer)
         ("C-x f s-i" . my-edit-init-file)
         ;;("C-x f l" . write-a-function-quickly)
         ("C-x f M-i" . my-edit-notes-file)
         ("C-x f E" . my-browse-emacs-dot-files-defuns)
         ("C-x f M-r" . rename-buffer)
         ("C-x f n" . find-next-file)
         ("C-x f C-r" . recover-this-file)
         ("C-x f p" . find-previous-file)
         ("C-x f /" . save-in-tmp-dir)
         ("C-x f 2" . spacemacs/find-file-split)
         ("C-x f 3" . spacemacs/find-file-vsplit)
         ("C-x C-M-d" . my-dired-to-downloads))
  ;;daanturo-open-files-with-mode-in-dir maybe
  :config
  (advice-add 'find-file :around #'find-file--line-number)
  (add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs))

(leaf directory-extras ; todo autoloads explained
  :require t)

(leaf ediff
  :leaf-defer t
  :custom ((ediff-window-setup-function . 'ediff-setup-windows-plain)
           (ediff-diff-options . "-w")
           (ediff-show-clashes-only . t)
           (ediff-split-window-function . 'split-window-horizontally)))

(leaf trashed
  :ensure t
  :bind ("C-x f ." . trashed)
  :custom ((trashed-action-confirmed . 'y-or-n-p)
           (trashed-user-header-line . t)
           (trashed-sort-key . '("Size" . nil))
           (trashed-date-format . "%Y-%m-%d %H:%M:%S")))

(leaf recentf
  :ensure t
  :require t
  :global-minor-mode recentf-mode
  :custom ((recentf-max-menu-items . 200)
           (recentf-max-saved-items . 6000)
           (recentf-auto-cleanup . 'never))

  :config
  (setq recentf-exclude `(,tramp-file-name-regexp
        "recentf"
        "/elpa/"
        "/elisps/"
        "\\`/tmp/"
        "/\\.git/"
        "/\\.cask/"
        "/tmp/gomi/"
        ".loaddefs.el"
        "/\\.cpanm/"
        "\\.mime-example"
        "\\.ido.last"
        "woman_cache.el"
        "\\`/proc/"
        "\\`/sys/"
        "/ssh\\(x\\)?:"
        "/su\\(do\\)?:"
        "^/usr/include/"
        "/TAGS\\'"
        "COMMIT_EDITMSG\\'"
        "CMakeCache.txt"
        "/bookmarks"
        "\\.gz$"
        "COMMIT_EDITMSG"
        "MERGE_MSG"
        "git-rebase-todo"))
  (recentf-load-list))

;; COMMIT: REMOVE 'ellocate' as pointless
;; COMMIT: REMOVE 'imenu' as pointless
