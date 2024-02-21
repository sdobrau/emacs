(leaf powershell
  :ensure t
  ;; load if loading a ps-related file
  :mode ("\\.ps1\\'"
         "\\.psm1\\'"
         "\\.psd1\\'"
         "\\.ps1xml\\'"
         "\\.pssc\\'"
         "\\.psrc\\'"
         "\\.cdxml\\'")
  :if (eq system-type 'windows-nt)
  ;; otherwise load on start if on windows machine
  :require t)
