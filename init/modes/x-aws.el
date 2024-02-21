(leaf cfn-mode
  :ensure t
  :mode)

(leaf flycheck-cfn
  :ensure t
  :require t
  :hook (cfn-mode-hook . (lambda () (setq flycheck-checkers (append flycheck-checkers '(cfn-lint cfn-nag))))))

