;; jinnovation
(setq tex-newline-function 'reindent-then-newline-and-indent)

(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'TeX-narrow-to-group 'disabled nil)

(leaf tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :hook ((TeX-mode-hook . flycheck-mode)
         (LaTeX-mode-hook . 'auto-fill-mode))
  :custom ((TeX-auto-save . t)
     (TeX-auto-untabify . t)
     (TeX-electric-escape . t)
     ;; parse file after loading it
     (TeX-parse-self . t)
     (TeX-view-evince-keep-focus . t))
  :config
  ;; AUCTeX queries for name of master file
  (setq-default TeX-master nil))

(leaf tex-mode
  :hook (TeX-mode-hook . latex-electric-env-pair-mode)
  :bind ((:latex-mode-map
    ("C-a" . beginning-of-line-text))
   ("C-c C-s" . nil)))

(leaf reftex
  :hook (LaTeX-mode-hook . 'turn-on-reftex))

(leaf xenops
  :ensure t)
