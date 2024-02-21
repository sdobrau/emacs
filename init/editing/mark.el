(setq-default set-mark-command-repeat-pop t)

(leaf expand-region
  :ensure t
  :after yaml-mode
  :require t
  :bind (("C-2" . er/expand-region)
         ("C-1" . er/contract-region)
         (:yaml-mode-map
          (("C-c m l" . er/mark-yaml-list-item)
           ("C-c m o" . er/mark-yaml-outer-block)
           ("C-c m i" . er/mark-yaml-inner-block)
           ("C-c m v" . er/mark-yaml-key-value)
           ("C-c m b" . er/mark-yaml-block)))))

(leaf phi-rectangle
  :ensure t
  :bind ("C-x SPC" . phi-rectangle-set-mark-command))
