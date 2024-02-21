(leaf ob-yaml
  :require t ;; local
  :custom (org-babel-k8s-command . "kubectl"))

(leaf ob-powershell
  :after org
  :ensure t
  :require t)
