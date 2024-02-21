(leaf occur-context-resize
  :ensure t
  :hook (occur-mode-hook . occur-context-resize-mode))

;;'loccur': show lines matching string in the current buffer.

(leaf loccur
  :ensure t
  :custom (loccur-jump-beginning-of-line . t)
  :bind (("M-s C-o" . loccur)
         ("M-s M-o" . loccur-current) ;; word-at-point
         ("M-s C-<" . loccur-previous-match)))

(leaf occur-extras
  :after daanturo-core-macros
  :bind (("M-s C-0" . daanturo-list|hide-not-matching|unmatched-lines|reversed-occur)
         (:occur-mode-map
          ("F" . kin-occur-flush-lines-containing-str)
          ("K" . kin-occur-keep-lines-containing-str))))

