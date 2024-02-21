(leaf irony
  :ensure t
  :preface
  (autoload 'file-remote-p "files")
  (defun irony-mode-disable-remote ()
    "Disabled irony in remote buffers."
    (when (and buffer-file-name
         (file-remote-p buffer-file-name))
      (irony-mode -1)))

  :hook ((c++-mode-hook c-mode-hook objc-mode-hook) . irony-mode-disable-remote)
  ((c++-mode-hook c-mode-hook objc-mode-hook) . irony-mode))

(leaf irony-cdb
  :ensure nil
  :hook (irony-mode-hook . irony-cdb-autosetup-compile-options))

(leaf flycheck-irony
  :ensure t
  :hook (flycheck-mode-hook . flycheck-irony-setup))

(leaf irony-eldoc
  :ensure t
  :hook (irony-mode-hook . irony-eldoc))

(leaf cmake-mode
  :ensure t
  :mode (("\\.cmake\\'" . cmake-mode)
         ("\\cmakelists.txt$" . cmake-mode)))

;; is automatically activated
(leaf cmake-font-lock
  :ensure t)

(leaf cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
   ("\\.m\\'" . c-mode)
   ("\\.c\\'" . c-mode)
   ("\\.cpp\\'" . c++-mode)
   ("\\.c++\\'" . c++-mode)
   ("\\.mm\\'" . c++-mode))
  :custom ((c-auto-newline . nil)))

(leaf rtags
  ;; managed by guix
  :hook ((c++-mode-hook c-mode-hook) . 'rtags-start-process-unless-running))
