(leaf x86-lookup
  :ensure t
  :preface
  (defun my-x86-lookup-view-with-emacs-in-last-pdf-window (pdf page)
    (interactive "P")
    (pop-to-buffer (first (list-buffers-with-mode 'pdf-view-mode)))
    (pdf-view-goto-page page)
    (xc/switch-to-last-window))
  :custom (x86-lookup-browse-pdf-function .
					  'my-x86-lookup-view-with-emacs-in-last-pdf-window)
  :config (setq x86-lookup-pdf (concat user-emacs-directory "x86.pdf"))
  :bind ("C-h x" . x86-lookup))

(leaf nasm-mode
  :ensure t
  :mode "\\.asm\\'")
