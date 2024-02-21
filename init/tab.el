(leaf tab-bar
  :custom ((tab-bar-new-button-show . nil)
           (tab-bar-close-button-show . nil)
           (tab-bar-show . nil)
           (tab-bar-new-tab-choice . #'new-empty-buffer)
           (tab-bar-format . '(tab-bar-format-history
                               tab-bar-format-tabs-groups
                               tab-bar-separator
                               tab-bar-format-add-tab))))

;; [[https://mihaiolteanu.me/emacs-workspace-management.html][molteanu]]
(defun tab-create (name)
  "Create a tab with name NAME if it doesn't exist already."
  (condition-case nil
      (unless (equal (alist-get 'name (tab-bar--current-tab))
                     name)
        (tab-bar-rename-tab-by-name name name))
    (error (tab-new)
           (tab-bar-rename-tab name))))

(leaf tab-line
  :ensure t
  :hook ((eww-mode-hook
          vterm-mode-hook) . tab-line-mode)
  :custom ((tab-line-new-button-show . nil)
           (tab-line-close-button-show . nil)
           (tab-line-show . nil)
           (tab-line-left-button . nil)
           (tab-line-right-button . nil)
           (tab-line-separator . nil)
           (tab-line-switch-cycling . t)
           (tab-line-new-tab-choice . 'vterm))
  (tab-line-tabs-function . #'tab-line-tabs-mode-buffers)
  ;; (tab-line-format . '(tab-line-format-history
  ;;                      tab-line-format-tabs-groups
  ;;                      tab-line-separator
  ;;                      tab-line-format-add-tab))
  ;; )
  :config
  (set-face-attribute 'tab-line-tab-current nil :inherit 'highlight :background)
  (set-face-attribute 'tab-line-tab-special nil :slant 'normal)
  (set-face-attribute 'tab-line nil :background (color-lighten-name (face-attribute 'default :background) 15)))

(leaf tab-line-extras
  :after tab-line
  :require t
  :bind ((:eww-mode-map
          (("C-<" . yao-kaz-tab-line-tab-previous)
           ("C->" . yao-kaz-tab-line-tab-next)))
         (:vterm-mode-map
          (("C-<" . yao-kaz-tab-line-tab-previous)
           ("C->" . yao-kaz-tab-line-tab-next))))

  :custom (tab-line-close-tab-function . #'tab-line-close-tab))
