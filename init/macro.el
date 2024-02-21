(leaf dmacro
  ;; doesn't work in general for some reason
  :ensure t
  :require t
  :global-minor-mode global-dmacro-mode
  :custom `((dmacro-key . ,(kbd "C-M-z"))))

(leaf elmacro
  :ensure t
  :require t
  :bind (("C-M-c m C-c" . elmacro-mode)
         ("C-M-c m !" . elmacro-show-last-macro)
         ("C-M-c m ?" . elmacro-show-last-commands)))

(leaf my-macros
:commands transpose-symbol-from-left-to-right
          transpose-symbol-from-right-to-left
:bind* (("M-<left>" . transpose-symbol-from-right-to-left)
  ("M-<right>" . transpose-symbol-from-left-to-right)))
