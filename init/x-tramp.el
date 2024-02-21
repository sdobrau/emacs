(setq tramp-completion-reread-directory-timeout nil
      remote-file-name-inhibit-cache nil)

(leaf tramp
  :require t
  :custom ((tramp-remote-process-environment . nil) ;; don’t use cache
           (tramp-completion-reread-directory-timeout . nil)
           (tramp-completion-use-auth-sources . nil)
           (remote-file-name-inhibit-cache . nil)
           ;;
           (tramp-ssh-controlmaster-options
            .
            "-o controlmaster=auto -o controlpath='~/.ssh/controlmasters/%%r@%%h:%%p' -o controlpersist=600")
           (tramp-use-connection-share . t)
           (tramp-use-ssh-controlmaster-options . t)
           (remote-file-name-inhibit-cache . t) ; don’t cache
           (tramp-histfile-override . nil) ; don’t use .tramp-history
           (tramp-verbose . 0)o3
           (tramp-default-method . "sudo")) ;; ssh otherwise
  :config
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)) ;; don’t backup tramp

  (setq tramp-remote-path
        (append tramp-remote-path
                '(tramp-own-remote-path))))

;; dakra
;;(dolist (tramp-proxies '((nil "\\`root\\'" "/s:%h:")
;;                        ((regexp-quote (system-name)) "root" "/sudo:%h:")
;;       ("localhost" nil nil)))
;;  (add-to-list 'tramp-default-proxies-alist tramp-proxies)))

;; TODO: what is this?
(leaf tramp-term
  :commands tramp-term
  :hook (tramp-term-after-initialized-hook
         .
         (lambda (host)
           (term-send-raw-string (concat "cd " default-directory (kbd "RET"))))))

;; TODO The following segemnts display the current buffer's =method=
;; and =user@host=.

;; (spaceline-define-segment ph/remote-method
;;                            (when (and default-directory
;;                                       (file-remote-p default-directory 'method))
;;                              (file-remote-p default-directory 'method)))
;;
;;(spaceline-define-segment ph/remote-user-and-host
;;                          (when (and default-directory
;;                                     (or
;;                                      (file-remote-p default-directory 'user)
;;                                      (file-remote-p default-directory 'host)))
;;                            (concat
;;                             (file-remote-p default-directory 'user) "@"
;;                             (file-remote-p default-directory 'host))))
;;Default faces for the tramp segments.

;; (defface ph/spaceline-tramp-user-host-face
;;   '((t :inherit 'mode-line
;;        :foreground "black"
;;        :background "#fce94f"))
;;   "Tramp User@Host Face"
;;   :group 'spaceline)

;; (defface ph/spaceline-tramp-method-face
;;   '((t :inherit 'mode-line
;;        :foreground "black"
;;        :background "#ff5d17"))
;;   "Tramp Method Face"
;;   :group 'spaceline)

(leaf tramp-auto-auth
  :ensure t
  :global-minor-mode tramp-auto-auth-mode
  :config
  (add-to-list 'tramp-auto-auth-alist
         '("root@10.1.255.182" .
           (:host "10.1.255.182"
            :user "root"
            :port "ssh"))))
