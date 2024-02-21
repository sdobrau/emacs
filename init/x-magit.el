(leaf magit
  :after buffer-extras
  quelpa magit
  :bind (("C-x g" . magit-status)
         ("C-x v C-c" . magit-commit-create)
         ("C-x v C-." . magit-commit-autofixup)
         ("C-x v M-." . magit-commit-reword)
         ("C-x v C-M-." . magit-commit-extend)
         ("C-x v SPC" . magit-push-current-to-upstream)
         (:magit-mode-map
          (("M-w" . easy-kill) ;; was key below v
           ("C-c s" . magit-jump-to-staged)
           ("C-c u" . magit-jump-to-unstaged)
           ("C-c M-w" . magit-copy-buffer-revision)
           ("C-o" . magit-dired-other-window)
           ("M-d" . daanturo-magit-diff-commit-or-branch-at-point-to-HEAD)
           ("M-k" . daanturo-magit-discard-no-trash)))
         (:magit-section-mode-map
          (("C-M-u" . magit-section-up))))
  :hook ((magit-post-stage-hook magit-post-refresh-hook) . my-recenter-top)
  :custom ((magit-repository-directories .
                                         '(("/home/strangepr0gram/media/garden/" . 6)))
           (magit-refresh-status-buffer . t)
           (magit-clone-set-remote.pushdefault . t)
           (magit-log-auto-more . t)
           (magit-commit-show-diff . t) ;; COMMIT: change to t
           ;; daanturo
           (magit-log-margin . '(t "%F %R" magit-log-margin-width t 16))
           ;; redguardtoo
           (magit-buffer-log-rags . '("--follow"))
           ;; don’t show index
           (magit-diff-adjust-tab-width . 'always)
           (magit-diff-refine-hunk . 'all) ; fine differences always
           ;; COMMIT: magit-revision-use-hash-sections
           (magit-revision-use-hash-sections . 'quickest) ;; search only 7 chars
           (magit-ediff-show-stash-with-index . nil)
           (magit-remote-add-set-remote.pushdefault . t)
           (magit-save-repository-buffers . 'dontask)
           (magit-blame-disable-modes . '(fci-mode view-mode yascroll-bar-mode))
           (magit-process-find-password-functions . '(magit-process-password-auth-source))
           (magit-process-password-prompt-regexps
            .
            '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
              "^\\(Enter \\)?[Pp]assword\\( for '?\\(https?://\\)?\\(?99:[^']*\\)'?\\)?: ?$"
              "Please enter the passphrase for the ssh key"
              "Please enter the passphrase to unlock the OpenPGP secret key"
              "^.*'s password: ?$"
              "^Yubikey for .*: ?$"
              "^Enter PIN for .*: ?$"
              "^\\[sudo\\] password for .*: ?$"))
           (magit-clone-default-directory . my-git-directory))
  :config
  ;; fix colors
  ;;(set-face-attribute 'magit-diff-removed-highlight nil :foreground (face-attribute 'diff-removed :foreground))
  ;;(set-face-attribute 'magit-diff-added-highlight nil :background (face-attribute 'diff-added :background))
  ;; DEBUG
  (magit-auto-revert-mode 0)
  (defun magit-dired-other-window ()
    (interactive)
    (dired-other-window (magit-toplevel)))

  ;; COMMIT: explicitly disable font-lock in magit-revision
  ;; (defun my-disable-font-lock (&optional arg)
  ;;   (interactive "P")
  ;;   (font-lock-mode 0))
  ;; (add-hook 'magit-revision-mode-hook #'my-disable-font-lock)
  (remove-hook 'git-commit-setup-hook #'with-editor-usage-message) ;; git-commit
  ;; kaz-yos: recenter when moving across sections/siblings
  (advice-add 'magit-section-forward :after #'my-recenter-top)
  (advice-add 'magit-section-forward-sibling :after #'my-recenter-top)
  (advice-add 'magit-section-backward :after #'my-recenter-top)
  (advice-add 'magit-section-backward-sibling :after #'my-recenter-top))
;; daanturo
;;(advice-add 'magit-process-insert-section
;;            :before
;;            #'magit-wiki-auto-display-magit-process-buffer)

(leaf my-magit-extras
  :after magit
  :commands (my-fast-magit-refresh-on
             my-fast-magit-refresh-off
             my-fast-magit-refresh-toggle)
  ;; proper commit history as accessible by M-n/M-p in commit/status buffers

  :bind (("C-x v !" . ar/magit-soft-reset-head~1)
         (:magit-mode-map
          (("C-x n s" . mw-magit-narrow-to-section)
           ("C-c SPC" . mw-magit-mark-section)
           ("C-x -" . lw-magit-checkout-last)))
         (:magit-repolist-mode-map
          (("w" . akirak/magit-repolist-kill-origin-url-at-point)
           ("D" . akirak/magit-repolist-trash-repository-at-point)
           ("R" . akirak/magit-repolist-rename-repository-at-point))))
  :custom ((magit-repolist-columns .
                                   '(("Path" 30 akirak/magit-repolist-column-path nil)
                                     ("Branch" 20 magit-repolist-column-branch nil)
                                     ("Drty" 4 akirak/magit-repolist-column-dirty nil)
                                     ("Unmg" 5 akirak/magit-repolist-column-unmerged nil)
                                     ("Stsh" 4 magit-repolist-column-stashes nil)
                                     ("B<U" 3 magit-repolist-column-unpulled-from-upstream
                                      ((:right-align t)
                                       (:help-echo "Upstream changes not in branch")))
                                     ("B>U" 3 magit-repolist-column-unpushed-to-upstream
                                      ((:right-align t)
                                       (:help-echo "Local changes not in upstream")))
                                     ("Date" 12 akirak/magit-repolist-column-commit-date nil)
                                     ("origin" 30 akirak/magit-repolist-column-origin nil))))
  :config
  (advice-add 'magit-status :after #'mw/advice/add-last-commit-messages))
;;  :bind (:magit-mode-map
;;         (("!")))

;; https://github.com/magit/forge/issues/363 "Selecting deleted buffer"
(leaf forge
  :after emacsql-sqlite closql magit
  :quelpa forge
  :bind ("C-x C-M-g" . forge-browse-repository))

(leaf magit-todos
  :quelpa (magit-todos :fetcher github
            :repo "alphapapa/magit-todos")
  ;;:hook (magit-status-mode-hook . magit-todos-mode)
  :custom ((magit-todos-update . t)
           (magit-todos-ignore-case . t)))

(leaf magit-difftastic
  :ensure nil)

;; TODO: wtf is wrong with this,
;; spec turns to nil against assq in
(leaf multi-magit
  :after my-magit-extras
  :custom ;; TODO: implement repolist columns very similar to ones in leaf magit
          (multi-magit-repolist-columns .
           '(("Name" 25 multi-magit-repolist-column-repo nil)
           ("Dirty" 5 multi-magit-repolist-column-status
            ((:right-align t)
             (:help-echo "N - untracked, U - unstaged, S - staged")))
           ("Branch" 10 magit-repolist-column-branch nil)
           ("Path" 99 akirak/magit-repolist-column-path nil))))

(leaf magit-commit-mark
  :after magit
  :ensure t
  ;; TODO: fetcher form for codeberg
  :hook (magit-mode-hook . magit-commit-mark-mode)
  :bind (:magit-log-mode-map :package magit-commit-mark
                             ((";" . magit-commit-mark-toggle-read)
                              ("M-;" . magit-commit-mark-toggle-star)
                              ("C-;" . magit-commit-mark-toggle-urgent)))
  :config
  (setq magit-commit-mark-directory
        (no-littering-expand-var-file-name "magit-commit-mark")))

(leaf magit-delta
  :after magit
  :quelpa magit-delta
  :hook (magit-mode-hook . magit-delta-mode))

(leaf magit-todos
  :ensure t
  :hook (magit-mode-hook . magit-todos-mode))
;; commit: wtf
(set-fringe-mode '(1 . 1))

;; TODO: emacs-pr-review
