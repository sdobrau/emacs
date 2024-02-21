;; TODO: take out and try with tree-sitter
(leaf smartparens
  :ensure t
  :preface
  (show-paren-mode -1) ;; global disable
  (autoload 'sp-local-pair "smartparens")
  (autoload 'sp-local-tag  "smartparens")
  :global-minor-mode show-smartparens-global-mode

  :hook (((emacs-lisp-mode-hook
	   inferior-emacs-lisp-mode-hook
	   ielm-mode-hook
	   lisp-mode-hook
	   inferior-lisp-mode-hook
	   lisp-interaction-mode-hook
	   slime-repl-mode-hook
	   eval-expression-minibuffer-setup-hook) . smartparens-mode)) ;; todo
  ;;(web-mode-hook . turn-on-smartparens-mode)
  ;;((html-mode-hook) . smartparens-mode))
  :bind (:smartparens-mode-map
	 (("C-M-f" . sp-forward-sexp) ;; navigation
	  ("C-M-b" . sp-backward-sexp)
	  ("C-M-u" . sp-backward-up-sexp)
	  ("C-M-d" . sp-down-sexp)
	  ("C-M-p" . sp-backward-down-sexp)
	  ("C-M-n" . sp-up-sexp)
	  ("M-s" . sp-splice-sexp) ;; depth-changing commands
	  ("M-<up>" . sp-splice-sexp-killing-backward)
	  ("M-<down>" . sp-splice-sexp-killing-forward)
	  ("M-r" . sp-splice-sexp-killing-around)
	  ("M-(" . sp-wrap-round)
	  ("C-)" . sp-forward-slurp-sexp) ;; barf/slurp
	  ("C-<right>" . sp-forward-slurp-sexp)
	  ("C-}" . sp-forward-barf-sexp)
	  ("C-<left>" . sp-forward-barf-sexp)
	  ("C-<right>" . sp-backward-slurp-sexp)
	  ("C-M-<left>" . sp-backward-slurp-sexp)
	  ("C-{" . sp-backward-barf-sexp)
	  ("C-M-<right>" . sp-backward-barf-sexp)
	  ("M-s" . sp-split-sexp)
	  ("M-s" . nil)
	  ("M-\\". sp-splice-sexp) ;; misc
	  ("M-j" . sp-join-sexp)))
  :custom ((sp-autoskip-closing-pair . 'always)
	   (sp-highlight-pair-overlay . nil)
	   (sp-show-pair-delay . 0.05)
	   (sp-show-pair-from-inside . t))

  :config
  (sp-local-pair 'python-mode "'" nil
		 :unless '(sp-point-before-word-p
			   sp-point-after-word-p
			   sp-point-before-same-p))
  (autoload 'sp-with-modes "smartparens" "" nil 'macro))

;; (sp-with-modes 'org-mode
;;   (sp-local-pair "*" "*"
;;                  :actions '(insert wrap)
;;                  :unless '(sp-point-after-word-p sp-point-at-bol-p)
;;                  :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
;;   (sp-local-pair "_" "_" :unless '(sp-point-after-word-p)
;;                  :wrap "C-_")
;;   (sp-local-pair "/" "/" :unless '(sp-point-after-word-p)
;;                  :post-handlers '(("[d1]" "spc")))
;;   (sp-local-pair "~" "~" :unless '(sp-point-after-word-p)
;;                  :post-handlers '(("[d1]" "spc")))
;;   (sp-local-pair "=" "=" :unless '(sp-point-after-word-p)
;;                  :post-handlers '(("[d1]" "spc")))
;;   (sp-local-pair "«" "»"))

;; (sp-with-modes '(java-mode c++-mode)
;;   (sp-local-pair "{" nil
;;                  :post-handlers '(("||\n[i]" "ret")))
;;   (sp-local-pair "/*" "*/"
;;                  :post-handlers '((" | " "spc")
;;                                   ("* ||\n[i]" "ret"))))
;; (sp-with-modes '(markdown-mode gfm-mode rst-mode)
;;   (sp-local-pair "*" "*" :bind "C-*")
;;   (sp-local-tag "2" "**" "**")
;;   (sp-local-tag "s" "```scheme" "```")
;;   (sp-local-tag "<"  "<_>" "</_>"
;;                 :transform 'sp-match-sgml-tags)

;;   (sp-local-pair 'emacs-lisp-mode "`" nil
;;                  :when '(sp-in-string-p))
;;   (sp-local-pair 'clojure-mode "`" "`"
;;                  :when '(sp-in-string-p))
;;   (sp-local-pair 'minibuffer-inactive-mode "'" nil
;;                  :actions nil))
;; (sp-with-modes 'nix-mode
;;   (sp-local-pair "'" "'"
;;                  :unless '(sp-in-comment-p
;;                            sp-in-string-quotes-p))
;;   (sp-local-pair "\"" "\"")
;;   (sp-local-pair "''" "''"
;;                  :unless '(sp-in-comment-p
;;                            sp-in-string-quotes-p)))

