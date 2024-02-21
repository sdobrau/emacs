(leaf frog-jump-buffer
  :require t
  :preface
  (defun my-frog-jump-buffer-most-recent-mode ()
    (interactive)
    (let ((filterfun #'frog-jump-buffer-filter-same-mode)
          (frog-jump-buffer-include-virtual-buffers t))
      (setq frog-jump-buffer-current-filter-function filterfun)
      (frog-jump-buffer)
      (setq frog-jump-buffer-current-filter-function nil)
      (setq frog-jump-buffer-include-virtual-buffers nil)))

  (defun my-frog-jump-buffer-mode-only ()
    (interactive)
    (let ((filterfun #'frog-jump-buffer-filter-same-mode))
      (setq frog-jump-buffer-current-filter-function filterfun)
      (frog-jump-buffer)
      (setq frog-jump-buffer-current-filter-function nil)))

  :bind (("M-a" . nil) ;; backward-sentence
         ("M-a" . frog-jump-buffer)
         ("M-q" . nil) ;; prog-fill-reindent-defun
         ("M-q" . my-frog-jump-buffer-most-recent-mode)))

(color-darken-name (face-attribute 'default :background) 10)
