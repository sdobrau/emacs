(defun my-yaml-mode-setup ()
  (interactive)
  (yaml-pro-mode)
  (local-set-key (kbd "M-g i") #'yaml-pro-jump)
  (highlight-indent-guides-mode)
  ;;(smart-shift-mode) ;; C-c <left right> TODO what?
  (local-set-key (kbd "C-<tab>") #'aj-toggle-fold)
  (local-set-key (kbd "<backspace>") #'backward-delete-char)
  (local-set-key (kbd "M-<backspace>") #'my-del-2-backwards)
  (aggressive-indent-mode -1)
  (local-set-key (kbd "RET") #'newline-and-indent)
  (local-set-key (kbd "M-RET") #'my-newline-and-indent-yaml-sequence)
  (smart-newline-mode -1)
  (local-set-key (kbd "C-c C-d") #'my-ansible-doc)
  (local-set-key (kbd "C-c >") #'indent-rigidly-right-to-tab-stop)
  (local-set-key (kbd "C-c <") #'indent-rigidly-left-to-tab-stop)
  (outshine-mode)
  (whitespace-cleanup-mode)
  (ispell-minor-mode -1)
  (local-set-key (kbd "{") #'my-tempel-insert-curly-brace))



(provide 'my-yaml-mode-setup)
