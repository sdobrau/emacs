(leaf alert
  :ensure t)

;; COMMIT: declare autothemer (as dependency of kaolin-themes)
(leaf autothemer
  :ensure t)

(leaf quick-peek ; flycheck-inline
  :ensure t
  :commands quick-peek-update
  quick-peek-hide
  quick-peek-overlay-contents
  quick-peek-overlay-ensure-at)


(leaf popup ; google-translate / define-it / imenu-popup
  :ensure t
  :commands popup-create
  :config
  (set-face-attribute 'popup-tip-face nil
                      :background (color-darken-name (face-attribute 'default :background) 1.8)
                      :foreground (color-lighten-name (face-attribute 'default :foreground) 1.2)))

(leaf posframe ; dired-posframe, gts-do-translate
  :ensure t)

(leaf popup-imenu
  :quelpa (popup-imenu
           :fetcher github
           :repo "ancane/popup-imenu"))
