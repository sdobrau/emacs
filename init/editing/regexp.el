(leaf visual-regexp
  :ensure t
  :after multiple-cursors
  :require t
  :custom (vr/engine . 'emacs))

(leaf visual-regexp-steroids
  :ensure t
  :bind (("M-%" . vr/replace)
         ("C-c M-%" . vr/mc-mark)
         ("C-M-s" . vr/isearch-forward)
         ("C-M-r" . vr/isearch-backward)))

