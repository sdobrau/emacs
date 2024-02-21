;; COMMIT: add frame packages

;; TODO: some simple startup..
;; TODO: make a leaf an place at end of init.
;; to start up with various

(leaf nameframe
  :ensure t
  :require t
  :bind ("C-x 5 n" . nameframe-create-frame))

(leaf nameframe-project
  :ensure t
  :require t
  :global-minor-mode nameframe-project-mode)

(leaf zoom-frame
  :ensure nil
  :bind (("C-x M-=" . acg/zoom-frame)
         ("C-x M--" . acg/zoom-frame-out)
         ("<M-wheel-up>" . acg/zoom-frame)
         ("<M-wheel-down>" . acg/zoom-frame-out)))
