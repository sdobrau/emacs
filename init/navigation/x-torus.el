;; COMMIT: add torus ..
(leaf torus
  :ensure t
  :require t
  :preface
  ;; TODO: C-u for new frame with torus->circle name and layout
  (defun my-torus-layout-grid ()
    (interactive)
    (torus-layout-menu ?g))
  (defun my-torus-layout-vertical ()
    (interactive)
    (torus-layout-menu ?v))
  (defun my-torus-layout-horizontal ()
    (interactive)
    (torus-layout-menu ?h))
  :custom ((torus-dirname . "~/.emacs.d/torus")
           (torus-load-on-startup . t)
           (torus-save-on-exit . t)
           (torus-maximum-horizontal-split . 20)
           (torus-autowrite-file . "~/.emacs.d/torus/torus.el")
           (torus-autoread-file . "~/.emacs.d/torus/torus.el"))
  :bind (:torus-map
         (("a" . torus-add-location)
          ("j" . torus-switch-location)
          ("p" . torus-previous-location)
          ("n" . torus-next-location)
          ("g" . my-torus-layout-grid)
          ("v" . my-torus-layout-vertical)
          ("h" . my-torus-layout-horizontal)
          ("C-a" . torus-add-circle)
          ("C-j" . torus-switch-circle)
          ("C-p" . torus-previous-circle)
          ("C-n" . torus-next-circle)
          ("M-a" . torus-add-torus)
          ("M-j" . torus-switch-torus)
          ("M-p" . torus-previous-torus)
          ("M-n" . torus-next-torus)
          ("i" . torus-info)))
  :config
  (define-keymap "s-x" 'torus-map))
