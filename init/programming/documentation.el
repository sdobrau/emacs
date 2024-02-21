;; Browse documentation
(leaf devdocs
  :ensure t
  :disabled t
  :require t
  :bind ((:devdocs-mode-map
    ((";" . devdocs-next-page)
     ("l" . devdocs-previous-page)
     ("M-<" . beginning-of-buffer)
     ("M->" . end-of-buffer)))
   (:sh-mode-map
    ("C-c ? " . devdocs-lookup))
   ("C-c ?" . devdocs-lookup)))

;; Tooltip
(leaf eldoc
  :hook (prog-mode-hook . eldoc-mode)
  :custom (eldoc-idle-delay . 2))

(leaf eldoc-overlay
  :ensure t
  :hook (eldoc-mode-hook . eldoc-overlay-mode)
  :custom (eldoc-overlay-delay . nil))
