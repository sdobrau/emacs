(leaf geiser-guile
  :custom ((geiser-repl-query-on-kill-p . nil)
           (geiser-repl-query-on-exit-p . nil)
           (geiser-repl-history-no-dups-p . nil)
           (geiser-scheme-implementation . "guile")
           (geiser-active-implementations . '(guile))
           (geiser-guile-binary . "/home/ccc/.guix-profile/bin/guile"))
  :mode ("\\.scm\\’" . geiser-mode)
  :config
  (setq-default geiser-guile-load-path '("/home/ccc/everything/git/guix")))

(leaf macrostep
  :ensure t)

(leaf macrostep-geiser
  :ensure t
  :after geiser-mode
  :hook ((geiser-mode-hook
	  geiser-repl-mode-hook) . macrostep-geiser-setup))

(leaf ac-geiser
  :hook ((geiser-mode-hook
	  geiser-repl-mode-hook) . ac-geiser-setup))
