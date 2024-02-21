(leaf electric
  :ensure t
  :hook ((prog-mode-hook . electric-quote-local-mode)
         (prog-mode-hook . electric-pair-local-mode)

         (text-mode-hook . electric-quote-local-mode)
         (text-mode-hook . electric-pair-local-mode)

         (org-mode-hook . electric-quote-local-mode)
         (org-mode-hook . electric-pair-local-mode)

         (message-mode-hook . electric-quote-local-mode)
         (message-mode-hook . electric-pair-local-mode)
         ;; (prog-mode-hook . electric-indent-local-mode)
         ;; insert newlines around some chars
         (prog-mode-hook . electric-layout-mode)
         (haskell-mode-hook . (lambda () (electric-indent-local-mode -1)))
         (nix-mode-hook . (lambda () (electric-indent-local-mode -1))))

  :custom ((electric-pair-inhibit-predicate .
            'electric-pair-conservative-inhibit)
           ;; help balance parens
           (electric-pair-preserve-balance . t)
           ;; pairs to be used regardless of major mode
           ;; curly single+double quotes and << >>
           (electric-pair-pairs . '((8216 . 8217)
                                    (8220 . 8221)
                                    (171 . 187)))
           ;; skip inserting chars if it helps balance
           ;; eg ) over () skips over )
           (electric-pair-skip-self . 'electric-pair-default-skip-self)
           ;; skip whitespace when skipping over closing parens
           (electric-pair-skip-whitespace . nil)
           ;; chars considered whitespace for above
           (electric-pair-skip-whitespace-chars . '(9 10 32))
           ;; replace ' with electric quote depending on context
           (electric-quote-context-sensitive . t)
           ;; for paragraphs
           (electric-quote-paragraph . t)
           ;; for strings
           (electric-quote-string . nil)
           ;; dont use curved double quotes. messes thingz up

           (electric-quote-replace-double . nil))
  :config
  (electric-pair-mode -1)
  (electric-quote-mode -1))

(leaf elec-pair
  :after smartparens
  :if (>= emacs-major-version 25)
  :hook (smartparens-mode-hook . (lambda () (electric-pair-local-mode -1))))

(leaf smart-hungry-delete
  :init (smart-hungry-delete-add-default-hooks)
  :ensure t
  :bind (;;("<backspace>" . smart-hungry-delete-backward-char)
         ;;("del" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :hook  ((sh-mode-hook . smart-hungry-delete-default-prog-mode-hook)
          (text-mode-hook .
                          smart-hungry-delete-default-text-mode-hook)))

(leaf replace
  :custom (list-matching-lines-jump-to-current-line . t)
  :hook (occur-mode-hook . hl-line-mode)
  :bind (:occur-mode-map
         (("t" . toggle-truncate-lines))))

