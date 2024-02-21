(leaf flx
  :ensure t
  :require t)

(leaf persistent-soft
  :after pcache list-utils)

(leaf simpleclip
  :ensure t
  :require t)

(leaf shell-switcher
  :ensure t)

;; COMMIT: add shell-maker as dependency of ChatGPT
(leaf shell-maker
  :ensure t)
