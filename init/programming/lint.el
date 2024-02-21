(leaf flycheck
  :ensure t
  :hook ((yaml-mode-hook . flycheck-mode)
         (emacs-lisp-mode-hook . flycheck-mode))
  ;; make it fast
  :custom ((flycheck-idle-change-delay . 3)
           (flycheck-idle-buffer-switch-delay . 3)
           (flycheck-display-errors-delay . 2)
           ;; use load path of current emacs session for checking
           (flycheck-emacs-lisp-load-path . 'inherit)
           (flycheck-relevant-error-other-file-show . nil)
           ;; when saving
           (flycheck-check-syntax-automatically . '(save))
           ;; navigate compilation errors, not standard errors with error
           ;; navigation keys
           (flycheck-standard-error-navigation . nil)
           (next-error-function . #'flycheck-next-error-function)
           (previous-error-function . #'flycheck-previous-error-function)))
;;(flycheck-global-modes . '(not eww-mode
;;        eshell-mode
;;        shell-mode
;;        eww-mode
;;        fundamental-mode
;;        erc-mode
;;        vterm-mode
;;        message-mode
;;        git-commit-mode
;;        view-mode
;;        text-mode
;;        org-mode)))) ;; never use in these

(leaf flycheck-inline
  :ensure t
  :after flycheck
  :hook (flycheck-mode-hook . flycheck-inline-mode)
  ;; use quick-peek for nice box display
  :init
  (setq flycheck-inline-display-function
        (lambda (msg pos err)
          (let* ((ov (quick-peek-overlay-ensure-at pos))
                 (contents (quick-peek-overlay-contents ov)))
            (setf (quick-peek-overlay-contents ov)
                  (concat contents (when contents "\n") msg))
            (quick-peek-update ov)))
        flycheck-inline-clear-function #'quick-peek-hide))

(leaf flycheck-color-mode-line
  :ensure t
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(leaf consult-flycheck
  :ensure t
  :after flycheck)
