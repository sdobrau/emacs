(leaf moom
  :ensure t
  :global-minor-mode moom-mode
  :bind (("M-0" . moom-undo)
         ("M-1" . moom-move-frame-left)
         ("M-2" . moom-move-frame-to-center)
         ("M-3" . moom-move-frame-right)
         ("C-M-1" . moom-change-frame-width-single)
         ("C-M-2" . moom-change-frame-width-double)
         ("C-M-`" . moom-change-frame-width-half-again)))

(leaf frame-center
  :require t
  :bind ("C-x 5 +" . ct/frame-center))

(leaf fwb-cmds
  :ensure t
  :require t
  :bind ("C-x 5 ^" . fwb-replace-current-window-with-frame))

(leaf frame-extras
  :require t)
