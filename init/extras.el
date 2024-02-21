(leaf text-extras
  :require t
  :bind ("C-M-c >" . bwf-cut-here))

(leaf little-extras
  :require t
  :bind (("s-<kp-home>" . time)
         ("s-<kp-7>" . time)
         ;("C-x f l" . )
         ("C-M-c l x" . mw-toggle-allx-mode)
        ("C-M-c l b y e" . suicide)))
