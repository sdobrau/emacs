;; * 'vc' and basic

;; COMMIT: add git-extras
(leaf git-extras
  :ensure nil
  :bind ("C-x v ^" . open-on-github))

(leaf git-modes
  :ensure t
  :mode (("/\\.gitattributes\\'"  . gitattributes-mode)
         ("/info/attributes\\'"   . gitattributes-mode)
         ("/git/attributes\\'"    . gitattributes-mode)

         ("/\\.gitconfig\\'"      . gitconfig-mode)
         ("/\\.git/config\\'"     . gitconfig-mode)
         ("/modules/.*/config\\'" . gitconfig-mode)
         ("/git/config\\'"        . gitconfig-mode)
         ("/\\.gitmodules\\'"     . gitconfig-mode)
         ("/etc/gitconfig\\'"     . gitconfig-mode)

         ("/\\.gitignore\\'"      . gitignore-mode)
         ("/info/exclude\\'"      . gitignore-mode)
         ("/git/ignore\\'"        . gitignore-mode)))

(leaf git-commit
  :hook (git-commit-mode-hook . (lambda () (setq-local fill-column 72)))
  :custom (git-commit-summary-max-length . 50))

(leaf vc
  :custom ((auto-revert-check-vc-info . nil) ;; TODO: wtf? find-file hook
           (vc-allow-async-revert . t)
           (vc-command-messages . t)
           (vc-make-backup-files . t)
           (version-control . 'never)
           ;; git diff switches
           (vc-git-diff-switches . '("-w" "-u3"))
           (vc-follow-symlinks . t)))
;; (vc-ignore-dir-regexp . '("\\(\\(\\`"
;;                           "\\(?:[\\/][\\/][^\\/]+[\\/]\\|/"
;;                           "\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)"
;;                           "\\'\\)\\|\\(\\`/[^/|:][^/|]*:\\)\\)\\|\\"
;;                           "(\\`/[^/|:][^/|]*:\\)"))))

(leaf vc-extras
  :after (magit magit-extras)
  :bind ("C-x v <" . redguardtoo-git-checkout-current-file))

;; * Extensions and others
;; COMMIT
(leaf git-link
  :ensure t
  :bind ("C-x v C-l" . git-link))

;; Hunk navigate
(leaf git-gutter
  :ensure t
  :require t
  :hook ((org-mode-hook prog-mode-hook) . git-gutter-mode)
  :custom (git-gutter:ask-p . nil)
  :bind (:vc-prefix-map
         (("[" . git-gutter:previous-hunk)
          ("]" . git-gutter:next-hunk)
          ("S" . git-gutter:stage-hunk)
          ("C-s" . git-gutter:set-start-revision)
          ("n" . git-gutter:revert-hunk)
          ("C-s" . git-gutter:set)
          ("SPC" . git-gutter:mark-hunk)
          ("C-c" . magit-commit-create)
          ("C-." . magit-commit-instant-fixup)
          ("C-M-.". magit-commit-extend)
          ("M-." . magit-commit-reword))))


;; COMMIT: add for dired support
(leaf dired-k2
  :quelpa (dired-k2
           :fetcher github
           :repo "syohex/emacs-dired-k2")
  :hook ((dired-initial-position-hook . dired-k2)))

;; Popup last commit of current-line
(leaf git-messenger
  :ensure t
  :bind ("C-x v ," . git-messenger:popup-message)
  :custom ((git-messenger:use-magit-popup . t)
           (git-messenger:show-detail . t)))

;; Time-machine of commit
(leaf git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine-toggle))

(leaf git-timemachine-extras
  :after git-timemachine
  :bind ("C-x v M-t" . redguardtoo-git-timemachine))

;; TODO: browse git repo locally

(leaf consult-ls-git
  :ensure t
  :bind ("C-x C-g" . consult-ls-git))

(leaf smeargle
  :ensure t
  :bind (("C-x v s" . smeargle)
         ("C-x v c" . smeargle-commits)))

;; (require 'vc-git)
;; (advice-add 'vc-git-find-file-hook :override
;;             (lambda ()
;;               "Activate `smerge-mode' if there is a conflict."
;;               (when (and buffer-file-name
;;                          (eq (vc-state buffer-file-name 'Git) 'conflict)
;;                          (save-excursion
;;                            (goto-char (point-min))
;;                            (re-search-forward "^<<<<<<< " nil 'noerror)))
;;                 (unless (and (boundp 'smerge-mode) smerge-mode)
;;                   (smerge-start-session))
;;                 (when vc-git-resolve-conflicts
;;                   (add-hook 'after-save-hook 'vc-git-resolve-when-done nil 'local))
;;                 (vc-message-unresolved-conflicts buffer-file-name))))

;; DEBUG
;; ;; COMMIT: consult-gh
;; (leaf consult-gh
;;   :quelpa (consult-gh
;;            :fetcher github
;;            :repo "armindarvish/consult-gh")
;;   :bind (("M-s C-g f" . consult-gh-search-repos)
;;          ("M-s C-g c" . consult-gh-repo-clone)))
