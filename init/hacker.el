(leaf ascii-table
  :ensure t)

(leaf buttonize-cves
  :after prot-eww
  :commands buttonize-buffer-with-cves)

(leaf uni-confusables
  :ensure t)

;; COMMIT: delete converter-module

(leaf rmsbolt
  :ensure t
  :hook (prog-mode-hook . rmsbolt-mode))
