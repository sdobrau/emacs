;; TODO: indent-tools https://github.com/vindarel/indent-tools
;; TODO: yaml https://blog.chmouel.com/2016/09/07/dealing-with-yaml-in-emacs/
;; TODO: python-yamllint for linting

(leaf yaml-mode
  :ensure t
  :mode "\\.yml\\'" "\\.yaml\\'" "\\.eyaml\\'")

(leaf yaml-mode-extras
  :after yaml-mode
  :bind (:yaml-mode-map
         (("<backtab>" . backward-indent))))

(leaf yaml-mode-setup
  :hook (yaml-mode-hook . my-yaml-mode-setup))

(leaf yaml
  :quelpa yaml
  :require t)

(leaf yaml-pro
  :require t
  ;; TODO: add to setup function
  :hook (yaml-mode-hook . yaml-pro-mode)
  :bind (:yaml-pro-mode-map
         (("C-M-e" . yaml-pro-next-subtree)
          ("C-M-a" . yaml-pro-prev-subtree)
          ("C-M-u" . yaml-pro-up-level)
          ("M-<up>" . yaml-pro-move-subtree-up)
          ("M-<down>" . yaml-pro-move-subtree-down))))

(leaf highlight-indent-guides
  :ensure t
  :hook (yaml-mode-hook . highlight-indent-guides-mode)
  :custom ((highlight-indent-guides-method . 'character)
           (highlight-indent-guides-responsive . 'stack)))

;; TODO: order in setup script
(leaf flycheck-yamllint
  :ensure t
  :hook (flycheck-mode-hook . flycheck-yamllint-setup))
