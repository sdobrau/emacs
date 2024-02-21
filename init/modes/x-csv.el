(leaf csv-mode
  :ensure t
  :mode "\\.csv\\'"
  :hook ((csv-mode-hook . (lambda () (visual-line-mode -1)))
         (csv-mode-hook . (lambda () (auto-fill-mode -1)))
         (csv-mode-hook . (lambda () (toggle-truncate-lines 1)))))
