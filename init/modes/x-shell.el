(leaf firestarter
  :ensure t)

;; COMMIT: remove old completion, set up pcmpl

(leaf term
  :custom ((term-input-autoexpand . t) ;; expand input based on history
           (term-input-ignoredups . t))) ;; ignore duplicates on input
