;; * AWS

(leaf terraform-mode
  :ensure t
  :mode "\\.tf\\'"
  :custom (terraform-indent-level . 4))

(leaf hcl-mode
  :ensure t
  :mode "\\.hcl\\'")

;; * Docker

(leaf docker
  :ensure t
  :bind ("C-c d" . docker))

(leaf dockerfile-mode
  :ensure t
  :mode "’Dockerfile\\’")


;; * Kubernetes
;; TODO:

(leaf kubernetes
  :ensure t)

;; * Puppet

(leaf puppet-mode
  :ensure t
  :mode "\\.pp\\'"
  :bind (:puppet-mode-map
         (("T" . nil))))

;; COMMIT REMOVE IF.
(leaf flymake-puppet
  :ensure t
  :hook (puppet-mode-hook . flymake-puppet-load))

;; * Ansible

(leaf ansible-doc
  :ensure t)

(leaf ansible-extras
  :commands my-ansible-mode-setup)

;; * Vagrant

(leaf vagrant
  :quelpa vagrant
  :commands (vagrant-destroy
             vagrant-edit
             vagrant-halt
             vagrant-provision
             vagrant-reload
             vagrant-resume
             vagrant-ssh
             vagrant-status
             vagrant-suspend
             vagrant-up))

(leaf vagrant-tramp
  :quelpa vagrant-tramp)
