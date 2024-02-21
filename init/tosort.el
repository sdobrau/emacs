(setq-default tags-revert-without-query t
	            redisplay-skip-fontification-on-input t ;; redguardtoo
	            inhibit-compacting-font-caches t)

;; COMMIT: rewrite setq-default in multiple separate

;; Emacs is allowed to read from processes
;; in chunks of maximum 80 MB.
(setq-default read-process-output-max (* 8192 1024))
(setq-default tags-case-fold-search nil)
(setq-default auto-revert-interval 99999999)
(setq-default tags-case-fold-search nil)
(setq-default wgrep-too-many-file-length 2024)
(setq-default auto-window-vscroll nil) ;; Performance - Sacha Chua

(remove-hook 'find-file-hook #'vc-refresh-state)
(remove-hook 'find-file-hook #'epa-file-find-file-hook)
(add-hook 'after-init-hook #'global-so-long-mode)
