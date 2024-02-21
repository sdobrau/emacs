(setq-default confirm-kill-processes nil)
(setq-default kill-buffer-query-functions nil)
(setq-default shell-command-switch "-c")
;; COMMIT: don’t display async commands’ output
(setq-default async-shell-command-display-buffer . nil)

(leaf quick-shell-keybind
  :ensure t
  :require t)

;; redirection supported, region supported
(leaf shell-command+
  :ensure t
  :bind ("M-!" . shell-command+)
  :custom (shell-command+-prompt . "$+: "))

(leaf xterm-color
  :ensure t
  :require t
  :commands xterm-color-colorize-buffer
  :custom ((xterm-color-use-bold-for-bright . t)))
