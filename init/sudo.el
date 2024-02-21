;; sudo edit key
(leaf sudo-edit
  :ensure t
  :bind ("C-x f s" . sudo-edit))

;; Auto sudo if non-readable
;;DEBUG
(leaf auto-sudoedit
  :ensure t
  :global-minor-mode auto-sudoedit-mode)

;; etc
(leaf sudo-utils
  :ensure t
  :bind (("C-M-c # #" . sudo-utils-program)
         ("C-M-c # !" . sudo-utils-shell-command)))
