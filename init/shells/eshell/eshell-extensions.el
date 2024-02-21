(leaf prot-eshell
  :after eshell
  :require t)

;; :bind (:eshell-mode-map
;;        (("C-c C-f" . prot-eshell-ffap-find-file)
;;         ("C-c C-j" . prot-eshell-ffap-dired-jump)
;;         ("C-c C-w" . prot-eshell-ffap-kill-save)
;;         ("C-c C->" . prot-eshell-redirect-to-buffer)
;;         ("C-c C-e" . prot-eshell-export)
;;         ("C-c C-r" . prot-eshell-root-dir))
;;        (:eshell-isearch-map
;;         (("C-c C-j" . prot-eshell-ffap-dired-jump)
;;          ("C-c C-w" . prot-eshell-ffap-kill-save)
;;          ("C-c C->" . prot-eshell-redirect-to-buffer)
;;          ("C-c C-e" . prot-eshell-export)
;;          ("C-c C-r" . prot-eshell-root-dir)))))


;; TODO: what is this?
(leaf esh-mode
  :after eshell
  :require t)


;; TODO: what is this?
(leaf em-smart
  :after eshell
  :require t)

;; Help functions for eshell. =C-h C-d= shows help for
;; command/function at point (e.g. man-page).

(leaf esh-help
  :ensure t
  :after eshell
  :require t)

;; With =eshell-bookmark= you can bind a bookmark to a basic shell in
;;a directory with the help of standard bookmarking keys. =C-x r m= to
;;set, =C-x r l= to restore.
;; Works with remote hosts via TRAMP.



(leaf eshell-fringe-status ;; use a simple -|+
  :ensure t
  :custom ((eshell-fringe-status-success-bitmap . 'efs-plus-bitmap)
           (eshell-fringe-status-failure-bitmap . 'efs-minus-bitmap)))


;; TODO: encrypted history?
(leaf em-hist
  :require t
  :custom ((eshell-hist-ignoredups . t)
           (eshell-history-size . 99999)))

;; fish-like completion support
(leaf pcmpl-args
  :ensure t
  :require t)

(leaf eshell-syntax-highlighting
  :ensure t
  :global-minor-mode eshell-syntax-highlighting-global-mode)

;; History autosuggestions
(leaf capf-autosuggest
  :ensure t
  :require t)

;; Visual commands in vterm
;; See 'eshell-visual-commands'
(leaf eshell-vterm
  :after vterm
  :ensure t
  :hook (vterm-mode . eshell-vterm-mode))

(leaf eshell-extras
  :require t
  :bind (:eshell-mode-map
         (("C-l" . eye/eshell-clear))))

;; COMMIT: add eshell-outline explicitly

(leaf eshell-outline
  :ensure t)

;; place explicitly after eshell-extras

(leaf eshell-mode-setup
  :after eshell-extras shell-switcher eshell-outline
  :require t
  :hook (eshell-mode-hook . eshell-mode-setup))
