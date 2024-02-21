(leaf consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x M-f" . consult-recent-file)
         ("C-h !" . consult-man)
         ("C-h i" . consult-info)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-x r b" . consult-bookmark) ;; bookmark+
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; COMMIT: remove consult-apropos
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g o" . consult-org-heading)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g M-i" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s M-o" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         (:eshell-mode-map
          (("M-g o" . consult-outline)))
         (:isearch-mode-map
          (("C-c h" . consult-isearch-history)
           ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
           ("M-s L" . consult-line-multi))))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section, after lazily
  ;; loading the package.

  :custom ((consult-async-input-debounce . 0.1)
           (consult-async-input-throttle . 0.1)
           (consult-async-min-input . 3)
           (consult-async-refresh-delay . 0.1)
           (consult-preview-key 'any)
           (consult--gc-threshold . most-positive-fixnum)
           (completion-in-region-function . #'consult-completion-in-region))

  :config
  ;;(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Optionally configure preview. The default value is 'any, such that any
  ;; key triggers the preview.
  ;; (setq consult-preview-key (kbd "M-."))

  (setq consult-preview-key '(:debounce 0.5 any))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                     ;;consult-bookmark
                     ;;consult-recent-file
                     ;;consult-xref
                     ;;consult--source-file why?
                     ;;consult--source-project-file why?
                     ;;consult--source-bookmark :preview-key '(kbd "M-.")
                     ;; COMMIT: fix consult-ripgrep form
                     consult-ripgrep :preview-key '(:debounce 0.3 any)
                     ;;consult-git-grep
                     consult-imenu
                     :preview-key '(:debounce 0.01 any)
                     consult-imenu-multi
                     :preview-key '(:debounce 0.01 any)
                     consult-line
                     :preview-key '(:debounce 0.01 any)
                     consult-org-heading
                     :preview-key '(:debounce 0.01 any)
                     consult-grep
                     :preview-key '(:debounce 0.1))

  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
    ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))
    ;;;; 2. projectile.el (projectile-project-root)
;; (autoload 'projectile-project-root "projectile")
;; (setq consult-project-root-function #'projectile-project-root)
    ;;;; 3. vc.el (vc-root-dir)
;; (setq consult-project-root-function #'vc-root-dir)
    ;;;; 4. locate-dominating-file
;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))

(leaf consult-extras
  :require t)

(leaf consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         (:minibuffer-local-completion-map
          (("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file)))))

(leaf consult-recoll
  :ensure t
  :after embark
  :config (consult-recoll-embark-setup))
