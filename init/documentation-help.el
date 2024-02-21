(setq apropos-do-all t ;; Search apropos more extensively.
      help-window-select t) ;; Always select the help buffer on pop-up

(leaf info
  :custom (info-lookup-other-window-flag . t)
  :bind (:Info-mode-map
         (("M-p" . backward-paragraph)
          ("M-n" . forward-paragraph))))

(leaf info-rename-buffer
  :ensure t
  :hook ((Info-mode-hook . info-rename-buffer-mode)))

(global-set-key (kbd "C-h M-s") #'shortdoc-display-group)

;; Better help buffer
(leaf helpful
  :ensure t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         ("C-c C-d" . helpful-at-point)
         (:helpful-mode-map
          (("q" . helpful-kill-buffers)
           ("g" . helpful-update))))
  :custom (helpful-switch-buffer-function . #'pop-to-buffer))

(leaf Man
  :commands man consult-man
  :custom ((Man-notify-method . 'pushy) ;; in pop-up frame
           (Man-width-max . nil)
           (Man-width . nil)))

;;(woman-imenu-generic-expression
;; . '((nil
;; "\n\\([a-z].*\\|\\cj.*\\)" 1)
;; ("*subsections*"
;;  "\\` \\{3,4\\}\\([a-z].*\\|\\cj.*\\)" 1)

;; alternative
(leaf woman
  :require t
  :custom ((woman-fill-frame . t)
           (woman-fill-column . 80)
           (woman-imenu . t)
           (woman-cache-level . 3))
  :bind ("C-h /" . woman)
  :config
  (setq woman-cache-filename
        (no-littering-expand-var-file-name "woman-cache.el")))


(leaf howdoyou
  :ensure t
  :bind ("M-s s C-s" . howdoyou-query))

(leaf arxiv-mode
  :ensure t
  :require t
  :custom ((arxiv-use-variable-pitch . nil)
           (arxiv-use-variable-pitch . nil))
  :bind (("C-M-c s a" . arxiv-search)
         ("C-M-c s M-a" . arxiv-complex-search))
  :config
  (setq arxiv-default-download-folder
        (no-littering-expand-var-file-name
         "arxiv/")))

(leaf eldoc
  :ensure t
  :custom (eldoc-idle-delay . 1)
  :hook (prog-mode-hook . eldoc-mode))

;; TODO: set-up
(leaf foldoc
  :config (setq foldoc-file (no-littering-expand-var-file-name "foldoc")))

;; Browse rfc-docs inside Emacs
(leaf rfc-mode
  :ensure t
  :require t
  :custom ((rfc-mode-use-original-buffer-names . t)
           (rfc-mode-browse-input-function . 'completing-read))

  :bind (:rfc-mode-map
         (("M-n" . rfc-mode-forward-page)
          ("M-p" . rfc-mode-backward-page))))

;; COMMIT: add org-recipes
(leaf org-recipes
  :require t
  :quelpa (org-recipes
           :fetcher github
           :repo "tuhdo/org-recipes")
  :bind ("C-h C-r" . org-recipes)
  :custom (org-wiki-location . nil)
  :config
  (add-to-list 'org-recipes-file-list (concat my-org-directory "recipes.org")))

;; COMMIT: configure define-word

;; TODO: bind to offline dictionary
(leaf define-word
  :ensure t
  :bind (("M-s w" . define-word)
         ("M-s C-w" . define-word-at-point)))

;;; OPENAI
;; COMMIT: add openai key
(leaf chatgpt-shell
  :ensure t
  :config
  ;;TODO: setup ai key
  (setq-default chatgpt-shell-openai-key
                (lambda ()
                  (auth-source-pass-get 'secret "openai-key"))))
