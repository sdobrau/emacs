;; * Main?

;; TODO: cleanup
;; COMMIT: declare global font lock and disable from magit-diff-mode
;; (leaf font-lock
;;   :ensure nil
;;   :global-minor-mode global-font-lock-mode)

(setq-default what-cursor-show-names t)

(setq-default indicate-empty-lines t
              what-cursor-show-names t
              text-quoting-style 'straight ;; use 'straight-quote'
              visible-bell nil ;; don't beep

              x-underline-at-descent-line t
              narrow-to-defun-include-comments t
              echo-keystrokes 0
              use-short-answers t
              mode-line-compact t)

(leaf atl-long-lines
  :ensure t
  :global-minor-mode atl-long-lines-mode)

(leaf visual-line
  :after shrface nov
  :hook ((shrface-mode-hook . turn-on-visual-line-mode)
         (shrface-mode-hook . (lambda () (setq-local
                                     org-startup-truncated nil)))))
                                        ;(display-fill-column-indicator-mode 1)

;; * Theming

(leaf macro-extras
  :require t)

(leaf kaolin-themes
  :ensure t
  :require t
  :init
  (defun my-mode-line-font-small ()
    (interactive)
    (set-face-attribute 'mode-line nil :height 85)
    (set-face-attribute 'mode-line-inactive nil :height 85))

  ;;COMMIT: remove my-toggle
  ;;(my-mode-line-font-small)
  :config
  (load-theme 'kaolin-mono-dark t nil)
  :custom ((kaolin-themes-hl-line-colored . nil)
           (kaolin-themes-bold . nil)
           (kaolin-themes-italic . nil)))

;; COMMIT: install solarized also
(leaf solarized-theme
  :ensure t
  :custom (solarized-use-less-bold . t))

;; slight variation in bg
(leaf solaire-mode
  :ensure t
  :disabled t)

;; * Cursor

(blink-cursor-mode 0)

;; DEBUG
;; (leaf smart-cursor-color
;;   :ensure t
;;   :global-minor-mode smart-cursor-color-mode)

(setq-default x-stretch-cursor nil
              cursor-in-non-selected-windows nil)

;; * Filling and fringes

(leaf modern-fringes
  :ensure t
  :global-minor-mode modern-fringes-mode)

(setq-default fringe-mode '(0 .0))

(set-face-attribute 'fringe nil :inherit 'org-level-4)

(leaf virtual-auto-fill
  :ensure t
  :hook ((org-mode-hook Info-mode-hook . virtual-auto-fill-mode))
  :custom (virtual-auto-fill-fill-paragraph-require-confirmation . nil))

;; DEBUG
(leaf visual-fill-column
  :ensure t
  :hook (((visual-fill-line-mode-hook
           eww-after-render-hook
           ansible-doc-module-mode
           Info-mode-hook
           devdocs-mode-hook
           helpful-mode-hook
           help-mode-hook
           rfc-mode-hook
           erc-mode-hook
           telega-chat-mode-hook
           elp-results-mode
           nov-mode-hook) . visual-fill-column-mode))

  :custom ((visual-fill-column-fringes-outside-margins . t)
           (visual-fill-column-enable-sensible-window-split . t)
           (visual-fill-column-width . 80)
           (visual-fill-column-center-text . t)
           (visual-fill-column-extra-text-width . '(1 . 1))
           (visual-fill-column--use-split-window-parameter . t))
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

;; * Focusing

(leaf dimmer
  :ensure t
  :custom ((dimmer-adjustment-mode . :foreground)
           (dimmer-fraction . 0.7)
           (dimmer-use-colorspace . :cielab)
           (dimmer-watch-frame-focus-events . t)
           (dimmer-buffer-exclusion-regexps .
                                            '("^\\*[h|h]elm.*\\*$"
                                              "^\\*embark.*\\*$ "
                                              " \\*\\(lv\\|transient\\)\\*"
                                              "^\\*minibuf-[0-9]+\\*"
                                              "^.\\*which-key\\*$"
                                              "^.\\*echo.*\\*"
                                              "^.\\*corfu.*\\*"
                                              "^.\\*eldoc-box.*\\*")))

  :config
  (dimmer-configure-helm)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  ;; COMMIT: add posframe.
  (dimmer-configure-posframe)
  (dimmer-configure-org))

(leaf dimmer-extras
  :after dimmer
  :require t
  :config
  (my-dimmer-configure-corfu))

                                        ;(dimmer-configure-gnus))

(leaf focus
  :ensure t
  :custom (focus-mode-to-thing . '((prog-mode . defun)
                                   (text-mode . paragraph)
                                   (eww-mode . paragraph)
                                   (org-mode . paragraph)
                                   (org-src-mode . defun)
                                   (outline-mode . defun)
                                   (Info-mode . paragraph))))

;; * Mode-line

(leaf mode-line-extras
  :require t
  :custom (mode-line-compact . t))

(leaf which-func
  :disabled t
  :require which-func-extras
  :config (which-function-mode)
  :mode "\\.xml\\’"
  :hook (which-func-functions . nxml-where))

;; * Little & Extra

(defalias 'yes-or-no-p 'y-or-n-p)

(leaf page-break-lines
  :ensure t
  :global-minor-mode global-page-break-lines-mode)

(leaf alert
  :ensure t
  :commands alert
  :custom (alert-default-style . 'message))

(leaf default-text-scale
  :ensure t
  :global-minor-mode default-text-scale-mode
  :require default-text-scale-extras
  :bind (("C-x C--" . nil) ;; text-scale-adjust
         ("C-x C--" . default-text-scale-decrease)
         ("C-x C-M-=" . viewing-1)
         ("C-x C-=" . nil) ;; text-scale-adjust
         ("C-x C-=" . default-text-scale-increase)
         ("C-x C-M--" . viewing-2))
  :custom (default-text-scale-amount . 5))

(leaf line-spacing
  :custom (line-spacing . 0.0))

(leaf show-eol
  :ensure t)

;; COMMIT: remove statusbar

;; COMMIT: add dashboard
(leaf dashboard
  :ensure t
  :custom ((dashboard-items . '((recents  . 3)
                                (bookmarks . 3)
                                (projects . 3)))
           (dashboard-projects-backend . 'project-el)
           (dashboard-set-footer . nil)
           (dashboard-set-init-info . nil)
           (dashboard-show-shortcuts . nil)
           (dashboard-center-content . t)
           (dashboard-banner-logo-title . nil))
  :config

  (dashboard-setup-startup-hook)
  ;; for daemon buffers
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))
