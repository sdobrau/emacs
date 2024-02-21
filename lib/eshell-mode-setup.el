;;; mine
;;; utilities
;;;; karthink helpers for fish-completion  https://github.com/karthink/.emacs.d

(defun fish-completion--list-completions (raw-prompt)
  (mapcar (lambda (e)
            (string-match "\\`\\([^\t]*\\)\t?\\(.*\\)\\'" e)
            (propertize
             (match-string 1 e)
             'fish-completion--annotation (match-string 2 e)))
          (split-string
           (fish-completion--list-completions-with-desc raw-prompt)
           "\n" t)))

(defun fish-completion--annotate (cand)
  (when-let* ((pos (or
                    (next-single-property-change
                     0 'fish-completion--annotation cand) 0))
              (ann (get-text-property pos 'fish-completion--annotation cand)))
    (concat (propertize " " 'display '(space :align-to left)) ann)))



(defun fish-completion--provide-annotation-function (table)
  (nconc table (list :annotation-function #'fish-completion--annotate)))

;;;; esh-help helpers

(autoload 'esh-help-eldoc-command "esh-help")

(defun esh-help-turn-on ()
  (interactive)
  (setq-local eldoc-documentation-function
              'esh-help-eldoc-command)
  (setq eldoc-documentation-function
        'esh-help-eldoc-command)
  (eldoc-mode 1))

;;;; jao-eshell-completion-capf helpers

;; (defun jao-eshell-completion-capf ()
;;   (let ((c (bash-completion-dynamic-complete-nocomint
;;             (save-excursion (eshell-bol) (point))
;;             (point)
;;             t)))
;;     (when (and c (listp c))
;;       (append c '(:exclusive no)))))

;; (defun jao-eshell--add-bash-completion ()
;;   (setq completion-at-point-functions
;;         '(jao-eshell-completion-capf
;;           pcomplete-completions-at-point t)))

;;;; don’t know what this is for

(defun eshell-spawn-external-command (beg end)
  "Parse and expand any history references in current input."
  (save-excursion
    (goto-char end)
    (when (looking-back "&!" beg)
      (delete-region (match-beginning 0) (match-end 0))
      (goto-char beg)
      (insert "spawn "))))

;;;; eshell key bindings fuck up back end
;;;;; backend

(defun eshell-mode-keys-setup ()
  (define-key eshell-mode-map "M-s-n" 'eshell-forward-argument)
  (define-key eshell-mode-map "M-s-p" 'eshell-backward-argument)
  (define-key eshell-mode-map "M-DEL" 'eshell-up-directory)
  (define-key eshell-mode-map "M-k" 'eshell-kill-input)
  (define-key eshell-mode-map "\C-c\C-e" 'prot-eshell-export)
  (define-key eshell-mode-map "\C-c\C-r" 'prot-eshell-root-dir)
  (define-key eshell-isearch-map "\C-m" 'eshell-isearch-return)
  (define-key eshell-isearch-map [return] 'eshell-isearch-return)
  (define-key eshell-isearch-map "\C-r" 'eshell-isearch-repeat-backward)
  (define-key eshell-isearch-map "\C-s" 'eshell-isearch-repeat-forward)
  (define-key eshell-isearch-map "\C-g" 'eshell-isearch-abort)
  (define-key eshell-isearch-map [backspace] 'eshell-isearch-delete-char)
  (define-key eshell-isearch-map [delete] 'eshell-isearch-delete-char))

(defun prot-eshell-keys-setup ()
  (define-key eshell-mode-map "\C-c\C-f" 'prot-eshell-ffap-find-file)
  (define-key eshell-mode-map "\C-c\C-j" 'prot-eshell-ffap-dired-jump)
  (define-key eshell-mode-map "\C-c\C-w" 'prot-eshell-ffap-kill-save)
  ;;([?\M-b] [C-left])
  ;; get error for C-c C-r non-prefix key C-c C-r but it’s fine
  ;; look into representations of keybindings and how to make this better lol
  ;;(define-key eshell-mode-map "\C-c\C-RB" 'prot-eshell-redirect-to-buffer)
  (define-key eshell-mode-map "\C-c\C-e" 'prot-eshell-export)
  (define-key eshell-mode-map "\C-c\C-r" 'prot-eshell-root-dir)
  (define-key eshell-isearch-map "\C-c\C-j" 'prot-eshell-ffap-dired-jump)
  (define-key eshell-isearch-map "\C-c\C-w" 'prot-eshell-ffap-kill-save)
  (define-key eshell-isearch-map "C->" 'prot-eshell-redirect-to-buffer)
  (define-key eshell-isearch-map "\C-c\C-e" 'prot-eshell-export)
  (define-key eshell-isearch-map "\C-c\C-r" 'prot-eshell-root-dir))

(defun esh-help-keys-setup ()
  (define-key eshell-mode-map "\C-h\C-d" 'esh-help-run-help)
  (define-key eshell-isearch-map "\C-h\C-d" 'esh-help-run-help))

;; (defun eshell-extras-keys-setup ()
;;   (define-key eshell-mode-map "\C-s l" 'eye/eshell-clear))

;;;;; front-end

;;;###autoload
(defun eshell-mode-keys-setup-for-all-plugins ()
  (eshell-mode-keys-setup)
  (prot-eshell-keys-setup)
  ;; COMMIT: remove eshell-extras-keys-setup
  (esh-help-keys-setup))


;;; main hook

(defun eshell-mode-setup ()

  ;; my addition: rename buffer after running command

  (add-hook 'eshell-post-command-hook #'my-eshell-rename-buffer)
  ;; shell-switcher: eshells non-shell-switcher spawned are now manageable
  ;; with shell-switcher

  (shell-switcher-manually-register-shell)

  ;; eshell bookmark

  (eshell-bookmark-setup)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output. # daviwil

  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  ;; properly colorize buffer,
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; xterm color setup

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color
                                               eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color")

  ;; Save command history when commands are entered # daviwil
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)


  (setq-local scorll-margin 0)
  (setq-local corfu-auto-prefix 1)
  (setq-local corfu-auto-delay 0.8)
  (corfu-mode)
  ;; (jao-eshell--add-bash-completion)
  (esh-help-turn-on)
  (eshell-bookmark-setup)
  (eshell-fringe-status-mode)
  ;; (capf-autosuggest-mode)
  (eshell-outline-mode)
  (eldoc-mode 0) ;; don’t need it
  (advice-add 'eshell-isearch-backward :before #'widen)
  (advice-add 'eshell-outline-narrow :before #'end-of-buffer)
  (eshell-syntax-highlighting-mode)
  ;; COMMIT: temporarily disable fish-completion
  ;; (fish-completion-mode)
  ;;(tab-line-mode)
  ;; (advice-add #'pcomplete-completions-at-point
  ;;             :filter-return
  ;;             #'fish-completion--provide-annotation-function)

  ;; (advice-add #'jao-eshell-completion-capf
  ;; :filter-return #'fish-completion--provide-annotation-function))

  ;; (setq eshell-command-completion-function
  ;;       (lambda ()
  ;;         (pcomplete-here (karthink/eshell-fish-complete-commands-list))))
  (esh-help-turn-on)

  ;; dakra
  (setq-local imenu-generic-expression `(("Prompt" ,eshell-prompt-regexp 1)))
  ;; see minad/consult
  ;; https://github.com/minad/consult/wiki
  (setq-local outline-regexp eshell-prompt-regexp)
  ;; finally add the keybindings
  (eshell-mode-keys-setup-for-all-plugins))



(provide 'eshell-mode-setup)
