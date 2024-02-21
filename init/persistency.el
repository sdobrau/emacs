;; Save various rings and histories in =~/.config/emacs/data/savehist=.
;; COMMIT: refactor savehist
(leaf savehist
  :ensure nil
  :require t
  :custom ((history-length . t)
           (savehist-save-minibuffer-history . t)
           ;; what other variables to save?
           (savehist-additional-variables . '(search-ring
                                              regexp-search-ring
                                              kill-ring
                                              comint-input-ring
                                              sr-history-registry
                                              file-name-history
                                              tablist-name-filter
                                              winner-ring-alist
                                              mark-ring
                                              eshell-history-ring
                                              kmacro-ring)))
  :config (setq savehist-file
                (no-littering-expand-var-file-name "savehist"))
  (savehist-mode 1))

;; Save point history. Abbreviate file-names for confidentiality and make
;; backups of the master save-place file.
(leaf save-place
  :ensure nil
  :global-minor-mode save-place-mode
  :custom ((save-place-abbreviate-file-names . t)
           (save-place-limit . nil)
           (save-place-version-control . t))
  :config (setq save-place-file
                (no-littering-expand-var-file-name "save-place.el")))

;; Save place in PDF files.
;; COMMIT
(leaf saveplace-pdf-view
  :if (window-system)
  :ensure t
  :hook (pdf-view-mode-hook . save-place-mode))
