;; COMMIT: add xref-rst
(leaf xref-rst
  :ensure t
  :hook (rst-mode-hook . xref-rst-mode))
