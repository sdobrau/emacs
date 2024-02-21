(leaf sh-script
  :hook (sh-mode-hook . sh-electric-here-document-mode)
  :custom (sh-indentation . 2)
  :hook (sh-mode-hook
         .
         (lambda ()
           (add-hook
            'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p
            nil
            t))))

(leaf shelldon
  :ensure t
  :bind (:sh-mode-map
         (("C-x C-e" . shelldon-send-line-at-point)
          ("C-x C-r" . shelldon-send-region)
          ("C-x C-h" . shelldon-send-region))))

(leaf shelldon-extras
  :after shelldon
  :bind (:sh-mode-map
         (("C-x C-h" . my-shelldon-send-defun))))

(leaf shfmt
  :ensure t
  ;; COMMIT: remove -p check from shfmt
  :custom (shfmt-arguments . '("-i" "2"))
  ;; COMMIT: add shfmt on save
  :hook (sh-mode-hook . shfmt-on-save-mode))

(leaf flycheck-bashate
  :ensure t)

(leaf sh-mode-setup
  ;; COMMIT: remove after, autoload is ok
  :hook (sh-mode-hook . sh-mode-setup))
