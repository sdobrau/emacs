(leaf xref
  :ensure t
  :hook (xref-after-return-hook . recenter)
  :custom (xref-marker-ring . 100)) ; should be enough

(leaf ggtags
  :ensure t
  :hook (python-mode-hook . gtags-mode)
  :bind ((:python-mode-map
          (("M-." . ggtags-find-tag-dwim)))))
