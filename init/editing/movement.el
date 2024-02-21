(setq-default line-move-visual nil)

(leaf visible-mark
  :ensure t)

(leaf rings
  :custom ((global-mark-ring-max . 15000)
           (mark-ring-max . 1500)
           (kill-ring-max . 1500)
           (kill-do-not-save-duplicates . t)))

;; Pop mark again after being popped on subsequent c-spc
;; (setq set-mark-command-repeat-pop t)

;; Mark-ring movement in both directions.
;;
;; =C--=: toggle forward direction.
;; =C-u SPC x2=: reverse direction.
;; =C-SPC=: go towards direction.

(leaf bd-set-mark
  :bind ([remap set-mark-command] . bd-set-mark-command))

;; When popping mark, skip consecutive identical marks # koekelas
;; (define-advice pop-to-mark-command (:around (f) koek-mark/ensure-move)
;; (let ((start (point))
;; (n (length mark-ring)))
;; ;; Move point to current mark
;; (funcall f)
;; ;; Move point to previous marks in mark ring
;; (while (and (= (point) start) (> n 0))
;; (funcall f)
;; (setq n (1- n)))))

(leaf kill-ring-extras
  :require t)

;; The following keys modify the selection:
;;
;; 1. @: append selection to previous kill and exit. for example, m-w d @ will
;;    append current function to last kill.
;; 2. C-w: kill selection and exit
;; 3. +, - and 1..9: expand/shrink selection
;; 4. 0 shrink the selection to the initial size i.e. before any expansion
;; 5. SPC: cycle through things in easy-kill-alist
;; 6. C-SPC: turn selection into an active region
;; 7. C-g: abort
;; 8. ?: help

(leaf movement-extras
  :require t
  :bind ((:prog-mode-map
          (([remap next-line] . zk-phi-next-line)
           ([remap previous-line] . zk-phi-previous-line)))
         (:text-mode-map
          (([remap next-line] . zk-phi-next-line)
           ([remap previous-line] . zk-phi-previous-line)))
         ;; 'python-mode-map' void error when loading this
         ;; do i have to?
         ;; (:python-mode-map
         ;;  (([remap next-line] . zk-phi-next-line)
         ;;   ([remap previous-line] . zk-phi-previous-line)))
         ("M-f" . koek-mtn/next-word)
         ("M-b" . koek-mtn/previous-word)))

;; =M-del=: delete subword.
;; =C-m-<backspace>=: delete superword.

(leaf subword
  :require t
  :global-minor-mode subword-mode
  :bind (("C-M-<backspace>" . backward-kill-superword)))

(leaf subword-extras
  :after subword
  :commands backward-kill-superword)

;; COMMIT: change keybindings
(leaf dogears
  :ensure t
  :after savehist
  :global-minor-mode dogears-mode
  :bind (("M-g ." . dogears-go)
         ("M-g l" . dogears-back)
         ("M-g ;" . dogears-forward)
         ("M-g ?" . dogears-list)
         ("M-g -" . dogears-sidebar))

  :custom ((dogears-idle . 3)
           (dogears-limit . 2000))
  :config
  ;; https://github.com/alphapapa/dogears.el/issues/4
  (add-to-list 'savehist-additional-variables 'dogears-list)
  (add-to-list 'dogears-ignore-modes 'eww-mode))

;; COMMIT: change goto-address with simpler global-goto-address-mode
(leaf goto-address
  :ensure nil
  :hook (global-goto-address-mode))

;; https://www.yahoo.com
;; https://www.google.com

(leaf goto-char-preview
  :ensure t
  :bind (("M-g c" . nil)
         ("M-g c" . goto-char-preview)))

(leaf goto-line-preview
  :ensure t
  :hook ((goto-line-preview-before-hook
          goto-line-preview-after-hook) . dimmer-mode)
  :bind (("M-g g" . nil)
         ("M-g g" . goto-line-preview)))

(leaf ace-jump-mode
  :ensure t
  ;; COMMIT: keybinding for ace-jump
  :bind ("C-c j" . ace-jump-char-mode)
  :custom (ace-jump-mode-gray-background . nil))

;; COMMIT: add `link-hint`
;; link navigation

;; for multiple types of links anywhere
(leaf link-hint
  :ensure t
  :bind (("C-c C-o" . link-hint-open-link)
         (:eww-mode-map
          (("F" . link-hint-open-link)))
         (:nov-mode-map
          (("F" . link-hint-open-link))))
  :custom ((link-hint-message . nil)
           (link-hint-restore . t)))

;; COMMIT: remove zzz-to-char, replace with ace-jump-zap
(leaf zzz-to-char
  :ensure t
  :after avy
  :custom (zzz-to-char-reach . 800)
  :bind ("M-z" . zzz-up-to-char))

(leaf smartscan
  :ensure t
  :hook (prog-mode-hook . smartscan-mode)
  :custom ((smartscan-symbol-selector . "symbol")
           (smartscan-use-extended-syntax . t)))

(leaf goto-last-change
  :ensure t
  :require t
  :bind ("M-g l" . goto-last-change-with-auto-marks))

(leaf beginend
  :ensure t
  :global-minor-mode beginend-global-mode)

;; COMMIT change keybindings
(leaf spatial-navigate
  :ensure t
  :bind (:prog-mode-map
         (("M-K" . spatial-navigate-backward-vertical-bar)
          ("M-J" . spatial-navigate-forward-vertical-bar)
          ("M-H" . spatial-navigate-backward-horizontal-bar)
          ("M-L" . spatial-navigate-forward-horizontal-bar))))

(leaf scroll
  :hook ((eshell-mode-hook
          erc-mode-hook
          telega-chat-mode-hook) . (lambda () (setq-local scroll-margin 0)
          (setq-local scroll-up-aggressively 0.0))))

(setq-default scroll-conservatively 10000
              maximum-scroll-margin 0.5
              scroll-error-top-bottom nil
              ;; Preserve screen point position when scrolling
              scroll-preserve-screen-position t
              fast-but-imprecise-scrolling t
              ;; counter emacs sluggishness when scrolling very fast
              scroll-margin 5)
