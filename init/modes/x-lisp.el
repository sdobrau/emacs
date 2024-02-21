;; TODO: level-1 face
(leaf highlight-stages
  :ensure t
  :hook ((emacs-lisp-mode-hook lisp-mode-hook) . highlight-stages-mode))

(leaf lisp-extra-font-lock
  :ensure t
  :hook ((lisp-mode-hook emacs-lisp-mode-hook) . lisp-extra-font-lock-mode))

(leaf highlight-function-calls
  :ensure t
  :hook ((emacs-lisp-mode-hook help-mode-hook) . highlight-function-calls-mode))

;; TODO: why does it not work sometimes?
;;(advice-add 'elisp-demos-advice-helpful-update :after #'highlight-function-calls-mode)
