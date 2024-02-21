;; Show keys on scren
(leaf keypression
  :ensure t)

;; Easily define repeat-map
(leaf repeaters
  :quelpa (repeaters
           :fetcher github
           :repo "mmarshall540/repeaters")
  :require t
  :config
  (repeaters-define-maps
   '(("git-gutter-with-magit"
      git-gutter:previous-hunk  "C-x v" "["
      git-gutter:next-hunk  "C-x v" "]"
      git-gutter:stage-hunk  "C-x v" "S"
      magit-commit-create "C-x v" "C-c"
      magit-commit-instant-fixup "C-x v" "C-."
      magit-commit-extend "C-x v" "C-M-."
      magit-commit-reword "C-x v" "M-.")))
  (repeat-mode))

;; ;; Frequency of keys
;; (leaf keyfreq
;; DEBUG
;;   :ensure t
;;   :require t
;;   :global-minor-mode keyfreq-mode keyfreq-autosave-mode
;;   :custom ((keyfreq-autosave-timeout . 3)
;;            (keyfreq-excluded-commands . '(self-insert-command
;;                                           abort-recursive-edit
;;                                           forward-char
;;                                           backward-char
;;                                           delete-backward-char
;;                                           execute-extended-command
;;                                           keyboard-quit
;;                                           kill-buffer
;;                                           left-char
;;                                           right-char
;;                                           right-word
;;                                           left-word
;;                                           save-buffer
;;                                           vertico-next
;;                                           vertico-previous
;;                                           isearch-cancel
;;                                           isearch-forward
;;                                           isearch-backward
;;                                           vertico-insert
;;                                           isearch-printing-char
;;                                           minibuffer-keyboard-quit
;;                                           isearch-repeat-forward
;;                                           isearch-repeat-backward
;;                                           scroll-up-command
;;                                           scroll-down-command
;;                                           vterm-send-c-l
;;                                           vterm-send-c-c
;;                                           vterm--self-insert
;;                                           previous-line
;;                                           next-line
;;                                           right-char
;;                                           previous-buffer
;;                                           org-self-insert-command
;;                                           helm-next-line))
;;            (keyfreq-excluded-regexp . '("^ace-jump-"
;;                                        "^backward-"
;;                                        "^company-"
;;                                        "^dired"
;;                                        "^evil-"
;;                                        "^forward-"
;;                                        "^general-dispatch-self-insert-command-"
;;                                        "^gnus-"
;;                                        "^ido-"
;;                                        "^isearch-"
;;                                        "^ivy-"
;;                                        "^keyfreq-"
;;                                        "^keyboard-"
;;                                        "^my-hydra-.*/body"
;;                                        "^next-"
;;                                        "^org-"
;;                                        "^paredit-"
;;                                        "^save-"
;;                                        "^scroll-"
;;                                        "^select-window-"
;;                                        "^undo-"
;;                                        "^web-mode"
;;                                        "^w3m-"
;;                                        "^yas-"
;;                                        "^y-or-n-"
;;                                        "emms-")))
;;   :config (setq keyfreq-file (no-littering-expand-var-file-name "keyfreq.el")))

;; Suggest free prefix keys
(leaf free-keys
  :ensure t
  :custom (free-keys-modifiers . '("s" "C" "M" "C-M", "C-M-S")))

;; COMMIT DELETED ORG-OPEN-AT-POINT


;; Key helper
(leaf which-key
  :ensure t
  ;; todo number-or-marker-p if which-key-mode
  :global-minor-mode which-key-mode
  :bind (("C-h C-k" . which-key-show-top-level)
         ("C-h M-k" . which-key-show-major-mode)
         ("C-h C-M-k" . which-key-show-full-keymap))
  :custom ((which-key-show-early-on-C-h . t)
           (which-key-idle-delay . 1.5)
           (which-key-max-description-length . 50)
           (which-key-allow-imprecise-window-fit . t) ; performance [redguardtoo]
           (which-key-separator . ": ")
           (which-key-idle-secondary-delay . 0.3)
           (which-key-min-display-lines . 6)
           (which-key-min-column-description-width . 30)
           (which-key-sort-order . 'which-key-description-order)
           (which-key-popup-type . 'side-window)
           (which-key-side-window-location . 'bottom)))
