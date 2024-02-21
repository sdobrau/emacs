;; yasnippet for everything else

(leaf yasnippet
  :ensure t
  :config (yas-reload-all)
  :hook (emacs-lisp-mode-hook . yas-minor-mode)
  :bind (("C-c s y n " . yas-new-snippet)
         ("C-<tab>" . yas/expand)))

;; TODO: check if dir exists. if not (new start) then move from package dir
(leaf yasnippet-snippets
  :ensure t
  :config
  (setq-default yas/root-directory (concat user-emacs-directory "data/snippets")
                yas-snippet-dirs (list (concat user-emacs-directory
                                               "data/snippets"))
                yasnippet-snippets-dir nil))

(leaf consult-yasnippet
  :after consult
  :ensure t
  :bind ("C-c s C-<tab>" . consult-yasnippet))

(leaf yasnippet-capf
  :ensure t
  :custom (yasnippet-capf-lookup-by . 'name) ;; or 'key
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(leaf auto-yasnippet
  :ensure t
  :config
  (setq aya-persist-snippets-dir (concat user-emacs-directory "data/snippets"))
  :bind (("C-c s a c" . aya-create)
         ("C-c s a e" . aya-expand)
         ("C-c s a h" . aya-expand-from-history)
         ("C-c s a d" . aya-delete-from-history)
         ("C-c s a n" . aya-next-in-history)
         ("C-c s a p" . aya-previous-in-history)
         ("C-c s a x" . aya-persist-snippet)
         ("C-c s a o" . aya-open-line)))

;; TODO: yasmate + textmate bundles

;; tempel for highly experimental

;; DEBUG
;; (leaf tempel
;;   :bind (([remap expand-abbrev] . tempel-expand)
;;          (:tempel-map
;;           (("M-n" . tempel-next)
;;            ("M-p" . tempel-previous)
;;            ("M-0" . tempel-abort)
;;            ("C-1" . tempel-insert))))
;;   :init

;; ;; setup completion at point
;; (defun tempel-setup-capf ()
;;   ;; add the tempel capf to `completion-at-point-functions'.
;;   ;; `tempel-expand' only triggers on exact matches. alternatively use
;;   ;; `tempel-complete' if you want to see all matches, but then you
;;   ;; should also configure `tempel-trigger-prefix', such that tempel
;;   ;; does not trigger too often when you don't expect it. note: we add
;;   ;; `tempel-expand' *before* the main programming mode capf, such
;;   ;; that it will be tried first.
;;   (setq-local completion-at-point-functions
;;               (cons #'tempel-expand
;;                     completion-at-point-functions)))

;; (add-hook 'yaml-mode-hook 'tempel-setup-capf)
;; (add-hook 'emacs-lisp-mode-hook 'tempel-setup-capf)

;; optionally make the tempel templates available to abbrev,
;; either locally or globally. `expand-abbrev' is bound to c-x '.
;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
;; (global-tempel-abbrev-mode)


;; (leaf tempel-extras
;;   :after tempel
;;   :require t)

;; TEST yankpad for complex setup
;; TODO setup sometime

(leaf yankpad
  :ensure t
  :custom (yankpad-file . "~/org/yankpad.org")
  :bind (("C-c s p C-<tab>" . yankpad-expand)
         ("C-c s p i" . yankpad-insert)))
;;:config
;; If you want to complete snippets using company-mode
;;(add-to-list 'company-backends #'company-yankpad)
;; If you want to expand snippets with hippie-expand
;;(add-to-list 'hippie-expand-try-functions-list #'yankpad-expand)))
