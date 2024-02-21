;; COMMIT: ensure nil for frame
(leaf frame
  :ensure nil
  :bind ("C-M-c f" . toggle-frame-fullscreen))

;; COMMIT: consistent keybinding for os-extras
(leaf os-extras
  :ensure nil
  :bind (("M-*" . exwm-shell-command)))

;; TODO: configure

;; The package desktop-environment provides commands and a global
;; minor mode to control your GNU/Linux desktop from Emacs.

;; With desktop-environment, you can control the brightness and
;; volume as well as take screenshots and lock your screen. The
;; package depends on the availability of shell commands to do the
;; hard work for us. These commands can be changed by customizing the
;; appropriate variables.

;; The global minor mode desktop-environment-mode binds standard keys
;; to provided commands: e.g., <XF86AudioRaiseVolume> to raise the
;; volume, <Print> to take a screenshot, and <s-l> to lock the screen.

(leaf desktop-environment
  :ensure t
  :require t
  :global-minor-mode desktop-environment-mode
  :config
  (defadvice desktop-environment-lock-screen
      (before change-bg-color activate)
    (custom-screenlock-command))
  (desktop-environment-mode))

(leaf desktop-environment-extras
  :require t
  :after desktop-environment)

;; Prettify store paths and do not auto-update (so as to minimize
;;            timers running in the
;;            background).

;; COMMIT: ensure nil for frame
(leaf proced
  :ensure nil
  ;;:hook (proced-mode-hook . guix-prettify-mode)
  :custom (proced-auto-update-flag . nil))

;; DISABLED
(leaf explain-pause-mode
  :disabled t
  :config (explain-pause-mode))

;; Disable the mouse.
(setq-default x-mouse-click-focus-ignore-position t)

;; Expectable behaviour of clipboard. Cut and paste uses the clipboard
;; and primary selection. When killing text outside Emacs, append it
;; to the clipboard as well.

(setq-default select-enable-clipboard t
              select-enable-primary t
              save-interprogram-paste-before-kill t)

(leaf direnv ;; grab
  :ensure t
  :commands direnv-mode
  :config (direnv-mode)
  :custom ((direnv-always-show-summary . t)
           (direnv-use-faces-in-summary . t)))

;; Not using it at the moment so not enabling it as a global minor mode.
(leaf envrc
  :ensure t
  :disabled t)

;; COMMIT: remove consult-project-extra already in consult

;; COMMIT: exec-path-from-shell please
(leaf exec-path-from-shell
  :ensure t
  :custom (exec-path-from-shell-variables . '("MANPATH" "PATH" "MYIP"))
  :config (exec-path-from-shell-initialize))

;; TODO: configure lemon
;; https://codeberg.org/emacs-weirdware/lemon


;;

(leaf empv
  :quelpa (empv
           :fetcher github
           :repo "isamert/empv.el")
  :custom ((empv-invidious-instance . "https://invidious.fdn.fr/api/v1")
           (empv-youtube-use-tabulated-results . t)))
