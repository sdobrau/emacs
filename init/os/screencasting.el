(leaf gif-screencast
  :ensure t
  :bind (("C-M-c ` g s" . gif-screencast)
   ("C-M-c ` g e" . gif-screencast-stop))
  :config
  (setq gif-screencast-output-directory
	(no-littering-expand-var-file-name "screencast/")))

(leaf escr
  :quelpa (escr
           :fetcher github
           :repo "atykhonov/escr")
  :commands escr-window-screenshot escr-frame-screenshot escr-region-screenshot
  :bind (("C-M-c ` s f" . escr-frame-screenshot)
         ("C-M-c ` s w" . escr-window-screenshot)
         ("C-M-c ` s r" . escr-region-screenshot))
  :config (setq escr-screenshot-directory
		(no-littering-expand-var-file-name "screenshots/")))

(leaf screencasting-extras
  :bind ("s-` o" . scrot))
