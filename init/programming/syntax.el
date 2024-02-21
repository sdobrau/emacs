;;; Parentheses

;; DEBUG
;; (leaf rainbow-delimiters
;;   :ensure t
;;   :hook ((emacs-lisp-mode-hook
;;           lisp-mode-hook
;;           shell-mode-hook) . rainbow-delimiters-mode))

(leaf show-paren
  :ensure nil
  :custom ((show-paren-style . 'parenthesis) ; show matchin paren
           (show-paren-when-point-in-periphery . t) ; show when near paren
           (show-paren-delay . 0.4)
           (show-paren-when-point-inside-paren . t)) ; don’t show when inside paren
  :hook (prog-mode-hook . show-paren-mode))

;;; Whitespace

(setq-default backward-delete-char-untabify-method 'hungry)

(leaf whitespace
  :custom
  ;; what to do when a buffer is visited or written
  ((whitespace-action . '(auto-cleanup))
   ;; which blanks to visualize?
   (whitespace-style . '(face tabs spaces newline space-mark tab-mark newline-mark empty trailing lines space-before-tab empty lines-style))
   ;; disable whitespace mode in these modes
   (whitespace-global-modes . '(not (erc-mode ses-mode)))
   ;; junkw emacs.d setup
   (whitespace-space-regexp . "\\( +\\|\x3000+\\)") ; mono and multi-byte space
   (whitespace-display-mappings
    .
    '((space-mark   ?\xa0   [?\u00a4]      [?_])
      (space-mark   ?\x8a0  [?\x8a4]       [?_])
      (space-mark   ?\x920  [?\x924]       [?_])
      (space-mark   ?\xe20  [?\xe24]       [?_])
      (space-mark   ?\xf20  [?\xf24]       [?_])
      (space-mark   ?\u3000 [?\u25a1])
      (newline-mark ?\n     [?$ ?\n])
      (TAB-mark     ?\t     [?\u00bb ?\t]  [?\\ ?\t])))
   ))

(leaf whitespace-cleanup-mode
  :ensure t
  :hook ((prog-mode-hook org-mode-hook) . whitespace-cleanup-mode)
  :custom (whitespace-cleanup-mode-only-if-initially-clean . nil)
  (whitespace-style
   . '(face trailing tabs spaces newline
            missing-newline-at-eof empty indentation
            space-after-tab space-before-tab
            space-mark tab-mark newline-mark)))

;;; Indent

(setq-default indent-tabs-mode nil
              c-basic-indent 2
              c-basic-offset 2
              sh-basic-offset 2
              ;; tab-stop positions are (2 4 6 8 ...)
              tab-stop-list (number-sequence 2 200 2)
              tab-width 2)

(paragraph-indent-minor-mode)

(leaf auto-fill-mode
  :after yasnippet
  :hook ((org-mode-hook
          prog-mode-hook
          org-src-mode-hook) . auto-fill-mode))

(leaf align
  :require t
  :ensure t
  :config
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode)))))

(leaf aggressive-indent
  :ensure t
  :hook (prog-mode-hook . aggressive-indent-mode)
  :custom ((aggressive-indent-dont-electric-modes . '(yaml-mode python-mode))
           (aggressive-indent-excluded-modes . '(python-mode text-mode))))

(leaf aggressive-indent-extras
  :after aggressive-indent
  :require t)

(leaf dtrt-indent
  :ensure t
  :global-minor-mode dtrt-indent-global-mode
  :custom ((dtrt-indent-verbosity . 0)
           ;; COMMIT: fix dtrt-indent and bash conflict
           ;; should fix issue with bash
           ;; dtrt-indent would send to smie-config-guess for sh which is wrong
           (dtrt-indent-run-after-smie . t)))

(leaf indent-tools
  :ensure t
  :bind (:yaml-mode-map
         (("M-<left>" . indent-tools-demote)
          ("M-<right>" . indent-tools-indent)
          ("C-M-f" . indent-tools-goto-next-sibling)
          ("C-M-b" . indent-tools-goto-previous-sibling)
          ("C-M-p" . indent-tools-goto-parent)
          ("C-M-n" . indent-tools-goto-child))))

;; Buttonize bug references
(leaf bug-reference
  :hook ((prog-mode . bug-reference-prog-mode)
         (text-mode . bug-reference-mode)))

;; Navigate to FIXME notices quickly
(leaf fixmee
  :ensure t
  :after button-lock nav-flash back-button smartrep string-utils
  tabulated-list)

(leaf fic-mode
  :ensure t
  :hook (prog-mode-hook . fic-mode))

;; COMMIT: remove tree-sitter. Moving to emacs 30

(leaf smart-newline
  :disabled t
  :hook (prog-mode-hook . smart-newline-mode))

;; 'a=10*5+2 > a = 10 * 5 + 2'
(leaf electric-operator
  :ensure t
  :hook ((python-mode-hook
          js2-mode-hook
          sql-mode-hook
          c-mode-hook
          php-mode-hook
          ruby-mode-hook) . electric-operator-mode)
  :custom ((electric-operator-enable-in-docs . nil)
           (electric-operator-double-space-docs . nil))) ;; "." -> ". "

(leaf aggressive-fill-paragraph
  :disabled t
  :hook (org-mode-hook . aggressive-fill-paragraph-mode))

(leaf fill-column
  :hook (((org-src-mode-hook
           prog-mode-hook
           emacs-lisp-mode-hook) . (lambda () (setq-local fill-column 80)
           (display-fill-column-indicator-mode)))
         (org-mode-hook . (lambda () (setq-local fill-column 80)))
         ((telega-root-mode-hook
           telega-chat-mode-hook) . (lambda () (setq-local fill-column 70)))
         (eww-mode-hook . (lambda () (setq fill-column 80)))))

;; COMMIT: remove reformatter, add apheleia
(leaf apheleia
  :ensure t)
