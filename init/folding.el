;; COMMIT: introduce folding
;; Indentation based using 'yafolding'
(leaf yafolding
  :ensure t
  :hook (prog-mode-hook . yafolding-mode)
  :bind (:prog-mode-map
         (("C-c f". yafolding-toggle-element)
          ("C-c C-a". yafolding-toggle-all)))
  :custom (yafolding-show-fringe-marks . nil))

;; Comment-based using 'bicycle'
(leaf bicycle
  :ensure t
  :hook ((prog-mode-hook . outline-minor-mode)
         (prog-mode-hook . hs-minor-mode))
  :bind (:outshine-mode-map ;; outshine outshines outline
         (("C-<tab>" . bicycle-cycle)
          ("C-c C-<tab>" . bicycle-cycle-global))))
