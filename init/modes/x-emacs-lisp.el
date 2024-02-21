;; [[https://karthinks.com/img/eval-last-sexp-demo-2.mp4]]
(leaf pp
:bind (([remap eval-expression] . pp-eval-expression)
       ([remap eval-last-sexp] . pp-eval-last-sexp)))

(leaf parinfer-rust-mode
  :disabled t
  :ensure t
  :hook ((emacs-lisp-mode-hook lisp-mode-hook)
          . (lambda () (progn (electric-pair-local-mode -1)
                              (parinfer-rust-mode))))
  :custom (parinfer-rust-preferred-mode . "paren"))

(leaf flylisp
  :ensure t
  :hook (emacs-lisp-mode-hook . flylisp-mode))

;; find references, C-u for specific directory only and not emacs-wide
(leaf elisp-refs
  :ensure t
  :bind (("C-c e v" . elisp-refs-variable)
         ("C-c e f" . elisp-refs-function)
         ("C-c e s" . elisp-refs-symbol)
         ("C-c e m" . elisp-refs-macro)
         ("C-c e s" . elisp-refs-special)))

(leaf eros
  :ensure t
  :hook ((org-mode-hook emacs-lisp-mode-hook) . eros-mode))

(leaf elisp-demos
  :ensure t
  :after helpful
  :config
  (advice-add 'helpful-update :after
              #'elisp-demos-advice-helpful-update))

;; highlight lexically-bound variables
(leaf lex-hl 
  :hook (emacs-lisp-mode-hook . lex-hl-mode)
  :bind (:lex-hl-mode-map
         (("M-s h C-`" . lex-hl-unhighlight)
          ("M-s h C-'" . lex-hl-top-level)
          ("M-s h C-," . lex-hl-prompt)
          ("M-s h C-." . lex-hl-nearest))))

(leaf elisp-def
  :ensure t
  :hook ((emacs-lisp-mode-hook ielm-mode-hook) . elisp-def-mode))

(leaf bug-hunter
  :ensure t)

(leaf elisp-lint
  :ensure t)
